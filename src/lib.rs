#![feature(nll, match_default_bindings)]

pub use deter::Deter;
pub use non_deter::NonDeter;
pub use push_down::PushDown;

/// A trait for structs that can accept some inputs and reject others.
pub trait Automaton {
    /// The type of the input alphabet.
    type Alphabet;

    /// Returnes whether the input is accepted by the automaton.
    fn accepts<I>(&self, input: I) -> bool
    where
        I: Iterator<Item = Self::Alphabet>;
}

mod deter;
mod non_deter;
mod push_down;
pub mod turing;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
