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
}
