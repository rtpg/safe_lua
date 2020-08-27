use super::*;

pub fn funcname<'a>(i: &'a IStream<'a>) -> IResult<&'a IStream<'a>, ast::Funcname> {

    let (i, (f_n, loc)) = name_with_loc(i)?;
    let (i, o_n_c) = many0(preceded(kwd("."), name))(i)?;
    let (i, m_m_c) = opt(preceded(kwd(":"), name))(i)?;
    return Ok((i, ast::Funcname {
        first_name_component: f_n,
        other_name_components: o_n_c,
        method_component: m_m_c,
	loc: loc
    }));
}


pub fn funcbody<'a>(i: &'a IStream<'a>) -> IResult<&'a IStream<'a>, ast::Funcbody> {
    let (i, k) = kwd("(")(i)?;
    let (i, m_parlist) = opt(parlist)(i)?;
    let (i, _) = kwd(")")(i)?;
    let (i, (b, _)) = pair(block, kwd("end"))(i)?;
    return Ok((i, ast::Funcbody {parlist: m_parlist, body: b, location: k.location}));
}

