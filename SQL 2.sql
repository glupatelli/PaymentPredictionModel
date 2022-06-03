WITH failed AS (
    SELECT 
            order_number
            , MIN(time) first_time_failed
FROM order_payment_history
WHERE status = 'payment_failed'
GROUP BY 1
    ),

paid AS (
        SELECT 
                order_number
                , time order_paid_time
FROM order_payment_history
WHERE status = 'order_paid'
    )

SELECT
        f.order_number
        , CASE
                WHEN order_paid_time IS NOT NULL THEN CAST(DATE_PART('day', 
order_paid_time - first_time_failed) AS TEXT)
                ELSE 'never_paid' 
        END AS failed_to_pay_days
FROM failed f
LEFT JOIN paid p
    ON f.order_number = p.order_number
