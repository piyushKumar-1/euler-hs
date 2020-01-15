

-- See https://www.juspay.in/docs/api/ec/#create-order


-- amount
-- Required	Double	Amount that the customer has to pay.
-- Will accept double values with upto two decimal places.
-- For example, 100.15 is valid input but 100.1532 is not valid.


-- currency	String	ISO string of the currency. Use INR for Indian Rupee. Among other accepted values are EUR, USD, GBP.
-- Default value: INR



-- customer_email	String	Email address of the customer.
-- If the backend gateway requires it, then you must send this value.

-- EHS: Backend gateway capabilities for better validation.
