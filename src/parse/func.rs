use super::*;

pub fn funcname(i: &IStream) -> IResult<&IStream, ast::Funcname> {
    let (i, f_n) = name(i)?;
    let (i, o_n_c) = many0(preceded(kwd("."), name))(i)?;
    let (i, m_m_c) = opt(preceded(kwd(":"), name))(i)?;
    return Ok((i, ast::Funcname {
        first_name_component: f_n,
        other_name_components: o_n_c,
        method_component: m_m_c,
    }));
}


pub fn funcbody(i: &IStream) -> IResult<&IStream, ast::Funcbody> {
    let (i, m_parlist) = surrounded(
        kwd("("),
        opt(parlist),
        kwd(")"))(i)?;
    let (i, (b, _)) = pair(block, kwd("end"))(i)?;
    return Ok((i, ast::Funcbody {parlist: m_parlist, body: b}));
}

