use crate::context::{EvalContext, Path, RetrieveAttribute};
use crate::expr::{Expr, ExprNode, ExpressionError};
use crate::float::{FloatExpr, FloatExprNode};
use crate::integer::{IntExpr, IntExprNode};
use num_traits::AsPrimitive;
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::sync::Arc;

/*impl Val {
    /// Attempt to cast the value to a specific numeric type
    pub fn cast_to<N>(self) -> Result<N, ExpressionError>
    where
        f64: AsPrimitive<N>,
        i128: AsPrimitive<N>,
        N: 'static + Copy,
    {
        match self {
            Val::F32(f) => Ok(f.as_()),
            Val::I32(i) => Ok(i.as_()),
        }
    }
}*/

pub trait ExprAttribute {
    type Property;
    type ExprType: ExprNode<Self::Property>;

    fn value(&self, ctx: &dyn EvalContext) -> Self::Property;
    fn lit(val: Self::Property) -> Expr<Self::Property, Self::ExprType>;
}

#[derive(Default, Debug)]
pub struct MapContext(pub HashMap<Path, HashMap<TypeId, Box<dyn Any>>>);

impl MapContext {
    pub fn insert_src<T: ExprAttribute + 'static>(&mut self, value: T::Property) {
        self.0
            .entry(Path("src".into()).to_owned())
            .or_insert(HashMap::new())
            .insert(TypeId::of::<T>(), Box::new(value));
    }

    pub fn insert_dst<T: ExprAttribute + 'static>(&mut self, value: T::Property) {
        self.0
            .entry(Path("dst".into()).to_owned())
            .or_insert(HashMap::new())
            .insert(TypeId::of::<T>(), Box::new(value));
    }
}

impl EvalContext for MapContext {
    fn get_any(&self, path: &Path, type_id: TypeId) -> Result<&dyn Any, ExpressionError> {
        let val = self.0.get(path).and_then(|m| m.get(&type_id)).unwrap();
        Ok(val.as_ref())
    }
}

#[derive(Debug)]
pub struct Health(Path);

impl Health {
    pub fn src() -> FloatExpr<f32> {
        let expr = FloatExprNode::Attribute(Box::new(Health("src".into())));
        Expr::new(Arc::new(expr))
    }

    pub fn dst() -> FloatExpr<f32> {
        let expr = FloatExprNode::Attribute(Box::new(Health("dst".into())));
        Expr::new(Arc::new(expr))
    }
}

impl RetrieveAttribute<f32> for Health {
    fn retrieve(&self, ctx: &dyn EvalContext) -> Result<f32, ExpressionError> {
        Ok(self.value(ctx))
    }
}

impl ExprAttribute for Health {
    type Property = f32;
    type ExprType = FloatExprNode<f32>;

    fn value(&self, ctx: &dyn EvalContext) -> Self::Property {
        let any = ctx.get_any(&self.0, TypeId::of::<Self>()).unwrap();

        let value = any
            .downcast_ref::<Self::Property>()
            .ok_or(ExpressionError::DowncastError)
            .unwrap();

        *value
    }

    fn lit(val: Self::Property) -> Expr<Self::Property, Self::ExprType> {
        Expr::new(Arc::new(Self::ExprType::Lit(val)))
    }
}

#[derive(Debug)]
pub struct Damage(Path);

impl Damage {
    pub fn src() -> IntExpr<i32> {
        let expr = IntExprNode::Attribute(Box::new(Damage("src".into())));
        Expr::new(Arc::new(expr))
    }

    pub fn dst() -> IntExpr<i32> {
        let expr = IntExprNode::Attribute(Box::new(Damage("dst".into())));
        Expr::new(Arc::new(expr))
    }
}

impl RetrieveAttribute<i32> for Damage {
    fn retrieve(&self, ctx: &dyn EvalContext) -> Result<i32, ExpressionError> {
        let any = ctx.get_any(&self.0, TypeId::of::<Self>())?;

        let value = any
            .downcast_ref::<i32>()
            .ok_or(ExpressionError::InvalidTypes)?;

        Ok(*value)
    }
}

impl ExprAttribute for Damage {
    type Property = i32;
    type ExprType = IntExprNode<i32>;

    fn value(&self, ctx: &dyn EvalContext) -> Self::Property {
        let any = ctx.get_any(&self.0, TypeId::of::<Self>()).unwrap();

        let value = any
            .downcast_ref::<Self::Property>()
            .ok_or(ExpressionError::InvalidTypes)
            .unwrap();

        *value
    }

    fn lit(val: Self::Property) -> Expr<Self::Property, Self::ExprType> {
        Expr::new(Arc::new(Self::ExprType::Lit(val)))
    }
}
