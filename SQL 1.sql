WITH filtered AS (
    SELECT 
    * 
FROM orders
WHERE CAST(delivery_date AS DATE) >= CAST('2020-02-08' AS DATE)
AND CAST(delivery_date AS DATE) <= CAST('2020-02-12' AS DATE)
AND country = 'Australia'
    ),

failed AS (
    SELECT 
    o.order_number
    , status
    , country
FROM order_payment_history o
LEFT JOIN filtered f
    ON o.order_number = f.order_number
WHERE status = 'payment_failed'
GROUP BY 1, 2, 3
)

SELECT 
    order_number
FROM failed
WHERE country IS NOT NULL
