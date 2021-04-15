use nom::sequence::{preceded, terminated};
use nom::IResult;

pub fn surrounded<T, U, V, I, O, O1, O2>(
    left_parser: T,
    inner: U,
    right_parser: V,
) -> impl Fn(I) -> IResult<I, O>
where
    T: Fn(I) -> IResult<I, O1>,
    U: Fn(I) -> IResult<I, O>,
    V: Fn(I) -> IResult<I, O2>,
{
    return preceded(left_parser, terminated(inner, right_parser));
}
