;;;; see <file:rabbitmq.asd> for loading RabbitMQ artifacts

(prove:plan 1)

(when (asdf:load-system :rabbitmq)
  (prove:ok
    (java:jnew-runtime-class
     "LispConsumer"
     :superclass "com.rabbitmq.client.DefaultConsumer"
     :constructors '((("com.rabbitmq.client.Channel")
                    (lambda (this channel)
                      (declare (ignore this channel))
                      1)))
     :methods '(("handleDelivery" :void
                 ("java.lang.String"
                  "com.rabbitmq.client.Envelope"
                  "com.rabbitmq.client.AMQP$BasicProperties"
                  (:array :byte))
                 (lambda (this consumer-tag envelope properties body)
                   (handler consumer-tag envelope properties body))))))
  (prove:diag "Create synthetic RabbitMQ consumer"))
    

(prove:finalize)


