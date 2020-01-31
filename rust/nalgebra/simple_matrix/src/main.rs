extern crate nalgebra;

use nalgebra as na;

fn main() {
    // Some matrices
    let all_zeros = na::Matrix2::from_element(0.);
    let identity = na::Matrix2::<f32>::identity();
    let still_identity = na::Matrix2::new(1., 0., 0., 1.);
}
