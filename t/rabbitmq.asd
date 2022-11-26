(defsystem rabbitmq
  :defsystem-depends-on (abcl-asdf)
  :components ((:mvn "com.rabbitmq/amqp-client/5.16.0")))
