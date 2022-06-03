WITH max_d AS (
    SELECT 
            customer_id
            , order_number
            , delivery_date
            , ROW_NUMBER() OVER(PARTITION BY customer_id ORDER BY delivery_date DESC) row_number
FROM orders
)

SELECT
        m.customer_id
        , c.name
        , c.phone
        , order_number
FROM max_d m
LEFT JOIN customers c
    ON m.customer_id = c.customer_id
WHERE row_number = 1