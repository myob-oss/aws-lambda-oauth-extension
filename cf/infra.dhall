let Function =
      https://github.com/jcouyang/dhall-aws-cloudformation/raw/0.7.57/cloudformation/AWS::Lambda::Function.dhall

let Fn =
      https://github.com/jcouyang/dhall-aws-cloudformation/raw/0.7.57/Fn.dhall

let JSON =
      https://github.com/dhall-lang/dhall-lang/raw/v21.0.0/Prelude/JSON/package.dhall

in  { AWSTemplateFormatVersion = "2010-09-09"
    , Description = "AWS Lambda OAuth Extension"
    , Resources.Function
      = Function.Resources::{
      , Properties = Function.Properties::{
        , Code = Function.Code::{ ImageUri = Some (JSON.string "ImageUri") }
        , Role = JSON.string "Role"
        }
      }
    }
