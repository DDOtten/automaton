#![feature(nll, match_default_bindings)]

//! An implementation of automatons in Rust.
//!
//! The main focus of this library is the [`Automaton`] trait. This trait features the [`accepts`]
//! method. This method returnes whether the input is accepted by the language the struct
//! represents.
//!
//! Each automata is implemented in a way where there are no invalid values for its variables.
//! Whatever values you give it is going to give a working automaton. Because of this the variables
//! of the automata can be public without violating safety.
//!
//! # Examples
//!
//! The finite automaton that accepts strings containing `a` and `b` that start with `bb` is shown
//! below:
//!
//! ```
//! //     ╭───╮  b  ╭───╮  b  ╔═══╗ ──╮
//! // ──▷ │ 0 │ ──▷ │ 1 │ ──▷ ║ 2 ║   │ a,b
//! //     ╰───╯     ╰───╯     ╚═══╝ ◁─┘
//! //           ╲ a   │
//! //            ╲    │ a
//! //             ◁   ▽
//! //               ╭───╮
//! //               │ 3 │
//! //               ╰───╯
//! //               △   │
//! //               ╰───╯
//! //                a,b
//!
//! use std::collections::{HashMap, HashSet};
//!
//! extern crate automaton;
//! use automaton::Automaton;
//!
//! # fn main() {
//! let mut initial_state = 0;
//!
//! let mut accepting_states = HashSet::new();
//! accepting_states.insert(2);
//!
//! let mut transitions = HashMap::new();
//! transitions.insert((0, 'a'), 3);
//! transitions.insert((0, 'b'), 1);
//! transitions.insert((1, 'a'), 3);
//! transitions.insert((1, 'b'), 2);
//! transitions.insert((2, 'a'), 2);
//! transitions.insert((2, 'b'), 2);
//! transitions.insert((3, 'a'), 3);
//! transitions.insert((3, 'b'), 3);
//!
//! let begins_with_bb = automaton::Deter {
//!     initial_state,
//!     accepting_states,
//!     transitions,
//! };
//!
//! assert!(begins_with_bb.accepts("bbb".chars()));
//! assert!(!begins_with_bb.accepts("abb".chars()));
//! # }
//! ```
//!
//! [`Automaton`]: trait.Automaton.html
//! [`accepts`]: trait.Automaton.html#tymethod.accepts

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
        I: IntoIterator<Item = Self::Alphabet>;
}

pub mod turing;

mod deter;
mod non_deter;
mod push_down;
