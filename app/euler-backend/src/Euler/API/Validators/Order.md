

See https://www.juspay.in/docs/api/ec/#create-order
See API tables & Gateway capabilities:
  https://docs.google.com/spreadsheets/d/10VeIlwTOpYI7xZJLe0RVaxsLVBtBLNgvtoh2JVH8Df4/edit?usp=sharing

Fields to validate:
------------------

- amount
  Amount that the customer has to pay.
  Will accept double values with upto two decimal places.
  For example, 100.15 is valid input but 100.1532 is not valid.

- currency
  ISO string of the currency. Use INR for Indian Rupee. Among other accepted values are EUR, USD, GBP.
  Default value: INR

- customer_email
  Email address of the customer.
  If the backend gateway requires it, then you must send this value.

- description
  Short description for the order. We send this information to the backend gateways whenever there is a provision for this.

- gateway_id
  Specify your preferred gateway for this order. Complete mapping for “gateway_id” can be found here: Gateway mapping

- return_url
  A fully qualified URL such as http://shop.merchant.com/ to which the customer will be redirected after payment completion. This URL shouldn’t contain any query parameters. This URL takes higher precedence over the common return URL configured in your account settings.

- billing_address_country_code_iso
  ISO Country code
  Default value: IND

- shipping_address_country_code_iso
  ISO Country code
  Default value: IND

- ...Other fields, see API & code
