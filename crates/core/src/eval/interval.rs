//! Confidence intervals for small-n eval rates (ADR-0025).
//!
//! Holds [`wilson`], the 95% Wilson score interval for a binomial proportion.
//! Eval rates are computed over a handful of items, where the textbook Wald
//! interval collapses to zero width at 0/n and n/n. Wilson stays honest there.
//! Pure arithmetic. It does not decide what a "good" rate is, and it assumes
//! independent items (correlated items make it too narrow).

/// The two-sided 95% normal quantile.
const Z_95: f64 = 1.96;

/// The 95% Wilson score interval `(lo, hi)` for `successes` out of `n`, or
/// `None` when `n` is 0 (there is no rate to bound).
pub fn wilson(successes: usize, n: usize) -> Option<(f64, f64)> {
    if n == 0 {
        return None;
    }
    let n_f = n as f64;
    let p = successes as f64 / n_f;
    let z2 = Z_95 * Z_95;
    let denom = 1.0 + z2 / n_f;
    let center = (p + z2 / (2.0 * n_f)) / denom;
    let half = Z_95 * (p * (1.0 - p) / n_f + z2 / (4.0 * n_f * n_f)).sqrt() / denom;
    // Clamp only float noise at the edges; the formula itself stays in [0, 1].
    Some(((center - half).max(0.0), (center + half).min(1.0)))
}

/// The 95% Newcombe score interval (1998, method 10) for the change
/// `p_second − p_first` between two **paired** proportions, or `None` with no
/// pairs (ADR-0029).
///
/// The arguments are the 2×2 counts of paired outcomes: `both` succeeded,
/// `only_first`, `only_second`, `neither`. It combines each arm's Wilson
/// interval with the phi correlation between arms, so it stays inside [−1, 1]
/// and is not degenerate when an arm is at 0% or 100%.
pub fn newcombe_paired(
    both: usize,
    only_first: usize,
    only_second: usize,
    neither: usize,
) -> Option<(f64, f64)> {
    let n = both + only_first + only_second + neither;
    let (lo_1, hi_1) = wilson(both + only_first, n)?;
    let (lo_2, hi_2) = wilson(both + only_second, n)?;
    let n_f = n as f64;
    let p_1 = (both + only_first) as f64 / n_f;
    let p_2 = (both + only_second) as f64 / n_f;
    let (a, b, c, d) = (
        both as f64,
        only_first as f64,
        only_second as f64,
        neither as f64,
    );
    // phi is undefined when any margin is empty; the interval then treats the
    // arms as uncorrelated, which Newcombe recommends.
    let margins = (a + b) * (c + d) * (a + c) * (b + d);
    let phi = if margins == 0.0 {
        0.0
    } else {
        (a * d - b * c) / margins.sqrt()
    };
    let change = p_2 - p_1;
    let below = ((p_2 - lo_2).powi(2) - 2.0 * phi * (p_2 - lo_2) * (hi_1 - p_1)
        + (hi_1 - p_1).powi(2))
    .max(0.0)
    .sqrt();
    let above = ((hi_2 - p_2).powi(2) - 2.0 * phi * (hi_2 - p_2) * (p_1 - lo_1)
        + (p_1 - lo_1).powi(2))
    .max(0.0)
    .sqrt();
    Some(((change - below).max(-1.0), (change + above).min(1.0)))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn close(got: (f64, f64), want: (f64, f64)) -> bool {
        (got.0 - want.0).abs() < 1e-3 && (got.1 - want.1).abs() < 1e-3
    }

    #[test]
    fn all_successes_at_small_n_stays_wide() {
        // Wald would say [1, 1] here.
        assert!(close(wilson(2, 2).unwrap(), (0.342, 1.0)));
    }

    #[test]
    fn no_successes_is_bounded_away_from_zero_width() {
        assert!(close(wilson(0, 1).unwrap(), (0.0, 0.793)));
    }

    #[test]
    fn half_is_symmetric() {
        assert!(close(wilson(1, 2).unwrap(), (0.095, 0.905)));
        assert!(close(wilson(5, 10).unwrap(), (0.237, 0.763)));
    }

    #[test]
    fn more_items_narrow_the_interval() {
        let (lo_small, hi_small) = wilson(5, 10).unwrap();
        let (lo_big, hi_big) = wilson(50, 100).unwrap();
        assert!(hi_big - lo_big < hi_small - lo_small);
    }

    #[test]
    fn bounds_stay_inside_zero_one() {
        for n in 1..=30 {
            for s in 0..=n {
                let (lo, hi) = wilson(s, n).unwrap();
                assert!((0.0..=1.0).contains(&lo) && (0.0..=1.0).contains(&hi) && lo <= hi);
            }
        }
    }

    #[test]
    fn no_items_has_no_interval() {
        assert_eq!(wilson(0, 0), None);
    }

    #[test]
    fn newcombe_matches_a_hand_computed_case() {
        // first 5/10, second 10/10, no item where only the first succeeded:
        // change +0.5, worked by hand from the method-10 formula.
        assert!(close(newcombe_paired(5, 0, 5, 0).unwrap(), (0.117, 0.763)));
    }

    #[test]
    fn perfect_agreement_on_few_items_stays_wide() {
        // Both arms 6/6: the change is 0, but six pairs cannot rule out ±39 points.
        assert!(close(newcombe_paired(6, 0, 0, 0).unwrap(), (-0.390, 0.390)));
    }

    #[test]
    fn a_complete_reversal_is_bounded_away_from_zero() {
        // First arm 4/4, second 0/4: the change is −1 and the interval excludes 0.
        assert!(close(newcombe_paired(0, 4, 0, 0).unwrap(), (-1.0, -0.307)));
    }

    #[test]
    fn newcombe_bounds_contain_the_estimate_and_stay_in_range() {
        for a in 0..4 {
            for b in 0..4 {
                for c in 0..4 {
                    for d in 0..4 {
                        let Some((lo, hi)) = newcombe_paired(a, b, c, d) else {
                            assert_eq!(a + b + c + d, 0);
                            continue;
                        };
                        let n = (a + b + c + d) as f64;
                        let change = (c as f64 - b as f64) / n;
                        assert!((-1.0..=1.0).contains(&lo) && (-1.0..=1.0).contains(&hi));
                        assert!(
                            lo <= change + 1e-12 && change <= hi + 1e-12,
                            "{a} {b} {c} {d}"
                        );
                    }
                }
            }
        }
    }
}
