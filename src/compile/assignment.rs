use crate::ast::{Expr, Varlist};

use super::{push_expr, push_var_assignment, Code, BC};

pub fn do_assignment<'a>(
    exprlist: &Vec<Expr<'a>>,
    varlist: &Varlist<'a>,
    code: &mut impl Code<'a>,
) {
    // x, y, z, alpha = f(), g()
    // we're going to call all of our expressions, then assign our values on the left
    // because we call our expressions one by one (backwards), we assign the variables afterwards
    // for example:
    //    call f
    //    call g
    //    assign_local_from_exprlist x 0
    //    pop
    //    assign_local_from_exprlist y 0
    //    assign_local_from_exprlist z 1
    //    assign_local_from_exprlist alpha 2
    //    pop

    let expr_count = exprlist.len();
    // push backwards so that the list is in the right order
    for i in (0..expr_count).rev() {
        push_expr(&exprlist[i], true, code);
    }
    // once we have the exprlist on the top of the stack
    // we'll try to assign to each variable
    let varlist_len = varlist.vars.len();
    for i in 0..varlist_len {
        let var = &varlist.vars[i];
        // first we extract out the value to assign...
        let exprlist_idx = if i < expr_count {
            0
        } else {
            i - expr_count + 1
        };

        code.emit(BC::EXTRACT_FROM_EXPRLIST(exprlist_idx), None);
        // then we do the assignment
        push_var_assignment(var, code);
        // if we still have expressions to go then we pop
        // x, y, z = f(), g() (on x assignment, y will use g so pop)
        if i < (expr_count - 1) {
            code.emit(BC::POP, None);
        }
        // otherwise we don't pop, cuz the last value will continue to be used
        // for right-wards expansion
    }
    // after all this we pop the last exprlist
    code.emit(BC::POP, None);
}
