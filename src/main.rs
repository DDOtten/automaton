#![allow(unused_imports, unused_variables)]

extern crate automaton;

use automaton::{Automaton, Deter, NonDeter};
use automaton::turing::{Direction, State};

fn main() {
    let turing = automaton::turing::TuringMachine {
        initial_state: 'o',
        transitions: vec![
            (('o', None), (State::Going('p'), None, Direction::Right)),
            (
                ('p', Some('a')),
                (State::Going('q'), Some('a'), Direction::Right),
            ),
            (
                ('p', Some('b')),
                (State::Going('s'), Some('b'), Direction::Right),
            ),
            (
                ('q', Some('a')),
                (State::Going('q'), Some('a'), Direction::Right),
            ),
            (
                ('q', Some('b')),
                (State::Going('r'), Some('b'), Direction::Right),
            ),
            (
                ('r', Some('a')),
                (State::Going('r'), Some('a'), Direction::Right),
            ),
            (
                ('r', Some('b')),
                (State::Going('r'), Some('b'), Direction::Right),
            ),
            (
                ('s', Some('a')),
                (State::Going('t'), Some('a'), Direction::Right),
            ),
            (
                ('s', Some('b')),
                (State::Going('s'), Some('b'), Direction::Right),
            ),
            (
                ('t', Some('a')),
                (State::Going('q'), Some('a'), Direction::Right),
            ),
            (
                ('t', Some('b')),
                (State::Going('r'), Some('b'), Direction::Right),
            ),
            (('t', None), (State::Accepted, None, Direction::Right)),
            (('r', None), (State::Accepted, None, Direction::Right)),
        ].into_iter()
            .collect(),
    };

    println!("{:?}", turing.accepts("aba".chars().map(|c| Some(c))));
    println!("{:?}", turing.accepts("abaa".chars().map(|c| Some(c))));
    println!("{:?}", turing.accepts("ba".chars().map(|c| Some(c))));
    println!("{:?}", turing.accepts("aaadba".chars().map(|c| Some(c))));
}
