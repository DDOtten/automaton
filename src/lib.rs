#![feature(nll, match_default_bindings, universal_impl_trait, conservative_impl_trait, dyn_trait)]

pub use deter::Deter;
pub use non_deter::NonDeter;
pub use push_down::PushDown;

use std::hash::Hash;
use std::collections::HashSet as Set;

pub trait Automaton {
    type State: Eq + Hash;
    type Alphabet: Eq + Hash;

    fn accepts<I: Iterator<Item = Self::Alphabet>>(&self, input: I) -> bool;

    fn states(&self) -> Set<Self::State>;
    fn labels(&self) -> Set<Self::Alphabet>;
}

#[doc(hidden)]
pub mod deter;

#[doc(hidden)]
pub mod non_deter;

#[doc(hidden)]
pub mod push_down;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
