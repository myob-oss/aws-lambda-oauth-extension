let Function =
      https://github.com/jcouyang/dhall-aws-cloudformation/raw/0.7.57/cloudformation/AWS::Lambda::Function.dhall sha256:0224e3e7a289b4c79bc9b5e12b46dcc463880a59fe9e1624b47a0b14aa041f49

let Role =
      https://github.com/jcouyang/dhall-aws-cloudformation/raw/0.7.57/cloudformation/AWS::IAM::Role.dhall sha256:3fbfda55dccf0b6728ea8d1e11f35c6a49dc47072a2f4dd9738a6534321aabe7

let Fn =
      https://github.com/jcouyang/dhall-aws-cloudformation/raw/0.7.57/Fn.dhall sha256:ce4b2ba3a418ca90c36d6162e8ea3b76bd8e883952a6e415bd1f4168526a4edc

let S = Fn.renderText

let JSON =
      https://github.com/dhall-lang/dhall-lang/raw/v21.0.0/Prelude/JSON/package.dhall sha256:5f98b7722fd13509ef448b075e02b9ff98312ae7a406cf53ed25012dbc9990ac

let Config = { name : Text, aloeConfig : Text, imageUri : Text }

in  \(config : Config) ->
      { AWSTemplateFormatVersion = "2010-09-09"
      , Resources =
        { Function = Function.Resources::{
          , Properties = Function.Properties::{
            , FunctionName = Some (S config.name)
            , Environment = Some Function.Environment::{
              , Variables = Some (toMap { ALOE_CONFIG = S config.aloeConfig })
              }
            , PackageType = Some (S "Image")
            , Code = Function.Code::{ ImageUri = Some (S config.imageUri) }
            , Role = Fn.render (Role.GetAttr.Arn "FunctionRole")
            }
          }
        , FunctionRole = Role.Resources::{
          , Properties = Role.Properties::{
            , AssumeRolePolicyDocument =
                JSON.object
                  ( toMap
                      { Version = JSON.string "2012-10-17"
                      , Statement =
                          JSON.array
                            [ JSON.object
                                ( toMap
                                    { Effect = JSON.string "Allow"
                                    , Principal =
                                        JSON.object
                                          [ { mapKey = "Service"
                                            , mapValue =
                                                JSON.string
                                                  "lambda.amazonaws.com"
                                            }
                                          ]
                                    , Action =
                                        JSON.array
                                          [ JSON.string "sts:AssumeRole" ]
                                    }
                                )
                            ]
                      }
                  )
            , RoleName = Some (S "${config.name}Role")
            , ManagedPolicyArns = Some
              [ S
                  "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"
              ]
            }
          }
        }
      }
