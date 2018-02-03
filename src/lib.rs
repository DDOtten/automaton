#![feature(nll, match_default_bindings)]

pub use deter::Deter;
pub use non_deter::NonDeter;
pub use push_down::PushDown;

use std::collections::HashSet as Set;

pub trait Automaton {
    type State;
    type Alphabet;

    type Situation;

    fn accepts<I>(&self, input: I) -> bool
    where
        I: Iterator<Item = Self::Alphabet>;

    fn traverse<I>(&self, input: I, situation: Self::Situation) -> Self::Situation
    where
        I: Iterator<Item = Self::Alphabet>;

    fn states(&self) -> Set<Self::State>;
    fn reachable_states(&self) -> Set<Self::State>;
    
    fn labels(&self) -> Set<Self::Alphabet>;
}

mod deter;
mod non_deter;
mod push_down;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
