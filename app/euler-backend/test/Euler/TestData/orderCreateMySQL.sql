-- MySQL dump 10.13  Distrib 5.7.29, for Linux (x86_64)
--
-- Host: mysql    Database: strip
-- ------------------------------------------------------
-- Server version	5.7.29

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `customer`
--

DROP TABLE IF EXISTS `customer`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `customer` (
  `id` varchar(255) NOT NULL,
  `version` bigint(20) NOT NULL,
  `date_created` datetime NOT NULL,
  `email_address` varchar(255) NOT NULL,
  `first_name` varchar(255) NOT NULL,
  `last_name` varchar(255) NOT NULL,
  `last_updated` datetime NOT NULL,
  `merchant_account_id` bigint(20) NOT NULL,
  `mobile_country_code` varchar(255) NOT NULL,
  `mobile_number` varchar(255) NOT NULL,
  `object_reference_id` varchar(128) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  UNIQUE KEY `merchant_account_id` (`merchant_account_id`,`object_reference_id`),
  UNIQUE KEY `ux_ref_id_merchant_id` (`object_reference_id`,`merchant_account_id`),
  KEY `FK24217FDE37947956` (`merchant_account_id`),
  KEY `object_reference_id` (`object_reference_id`),
  KEY `obj_ref_id_idx` (`object_reference_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `customer`
--

LOCK TABLES `customer` WRITE;
/*!40000 ALTER TABLE `customer` DISABLE KEYS */;
INSERT INTO `customer` VALUES ('cst_2h0dwktdpsefeyk6',0,'2017-12-01 12:42:53','test@gmail.com','','','2017-12-01 12:42:53',101,'91','7795296049','123456');
/*!40000 ALTER TABLE `customer` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ingress_rule`
--

DROP TABLE IF EXISTS `ingress_rule`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ingress_rule` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `version` bigint(20) NOT NULL,
  `date_created` datetime NOT NULL,
  `ip_address` varchar(255) NOT NULL,
  `last_updated` datetime NOT NULL,
  `merchant_account_id` bigint(20) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKFB18864637947956` (`merchant_account_id`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `ingress_rule`
--

LOCK TABLES `ingress_rule` WRITE;
/*!40000 ALTER TABLE `ingress_rule` DISABLE KEYS */;
INSERT INTO `ingress_rule` VALUES (2,1,'2019-06-04 17:43:44','192.168.1.1','2019-06-04 17:43:44',81);
/*!40000 ALTER TABLE `ingress_rule` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `merchant_account`
--

DROP TABLE IF EXISTS `merchant_account`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `merchant_account` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `version` bigint(20) NOT NULL,
  `bank_account_number` varchar(32) DEFAULT NULL,
  `credit_balance` double NOT NULL DEFAULT '0',
  `merchant_id` varchar(64) DEFAULT NULL,
  `merchant_name` varchar(64) DEFAULT NULL,
  `api_key` varchar(255) DEFAULT NULL,
  `express_checkout_enabled` bit(1) DEFAULT NULL,
  `inline_checkout_enabled` bit(1) DEFAULT NULL,
  `inline_checkout_html` longtext,
  `user_id` bigint(20) DEFAULT NULL,
  `alternative_email` varchar(255) DEFAULT NULL,
  `contact_person` varchar(255) DEFAULT NULL,
  `mobile` varchar(255) DEFAULT NULL,
  `telephone` varchar(255) DEFAULT NULL,
  `website` varchar(255) DEFAULT NULL,
  `icici_key` text,
  `icici_merchant_id` varchar(255) DEFAULT NULL,
  `icici_second_factor_id` varchar(255) DEFAULT NULL,
  `about_business` varchar(255) DEFAULT NULL,
  `admin_contact_email` varchar(255) DEFAULT NULL,
  `city` varchar(255) DEFAULT NULL,
  `contact_person_email` varchar(255) DEFAULT NULL,
  `contact_person_primary` varchar(255) DEFAULT NULL,
  `contact_person_secondary` varchar(255) DEFAULT NULL,
  `contact_person_secondary_email` varchar(255) DEFAULT NULL,
  `landmark` varchar(255) DEFAULT NULL,
  `office_line1` varchar(255) DEFAULT NULL,
  `office_line2` varchar(255) DEFAULT NULL,
  `office_line3` varchar(255) DEFAULT NULL,
  `state` varchar(255) DEFAULT NULL,
  `zip` varchar(255) DEFAULT NULL,
  `icici_verified` bit(1) DEFAULT NULL,
  `locker_id` varchar(15) DEFAULT NULL,
  `date_created` datetime NOT NULL,
  `last_modified` datetime NOT NULL,
  `axis_merchant_id` varchar(255) DEFAULT NULL,
  `secure_secret` varchar(255) DEFAULT NULL,
  `axis_verified` bit(1) DEFAULT NULL,
  `axis_secure_secret` varchar(255) DEFAULT NULL,
  `axis_access_code` varchar(255) DEFAULT NULL,
  `hdfc_merchant_id` varchar(255) DEFAULT NULL,
  `hdfc_password` varchar(255) DEFAULT NULL,
  `hdfc_verified` bit(1) DEFAULT NULL,
  `ebs_account_id` varchar(12) DEFAULT NULL,
  `ebs_hash` varchar(32) DEFAULT NULL,
  `ebs_verified` bit(1) DEFAULT NULL,
  `enable_reauthentication` bit(1) DEFAULT NULL,
  `enable_reauthorization` bit(1) DEFAULT NULL,
  `enable_recapture` bit(1) DEFAULT NULL,
  `enable_sending_last_four_digits` bit(1) DEFAULT NULL,
  `return_url` varchar(128) DEFAULT NULL,
  `domain` varchar(64) DEFAULT NULL,
  `whitelabel_enabled` bit(1) DEFAULT NULL,
  `payu_merchant_key` varchar(64) DEFAULT NULL,
  `payu_salt` varchar(64) DEFAULT NULL,
  `payu_verified` bit(1) DEFAULT NULL,
  `otp_enabled` bit(1) DEFAULT NULL,
  `get_card_enabled` bit(1) DEFAULT NULL,
  `reverse_card_enabled` bit(1) DEFAULT NULL,
  `reverse_token_enabled` bit(1) DEFAULT NULL,
  `hdfc_test_mode` bit(1) DEFAULT NULL,
  `real_mode_only` bit(1) DEFAULT NULL,
  `gateway_decided_by_health_enabled` bit(1) DEFAULT NULL,
  `gateway_priority` varchar(255) DEFAULT NULL,
  `amex_merchant_id` varchar(255) DEFAULT NULL,
  `amex_verified` bit(1) DEFAULT NULL,
  `must_use_given_order_id_for_txn` bit(1) DEFAULT NULL,
  `enable3dsecure_help_mail` bit(1) DEFAULT NULL,
  `amex_access_code` varchar(255) DEFAULT NULL,
  `amex_password` varchar(255) DEFAULT NULL,
  `amex_secure_secret` varchar(255) DEFAULT NULL,
  `amex_username` varchar(255) DEFAULT NULL,
  `paypal_client_id` varchar(255) DEFAULT NULL,
  `paypal_secret` varchar(255) DEFAULT NULL,
  `hdfc_ivr_merchant_id` varchar(255) DEFAULT NULL,
  `hdfc_ivr_password` varchar(255) DEFAULT NULL,
  `hdfc_ivr_verified` bit(1) DEFAULT NULL,
  `settlement_account_id` bigint(20) DEFAULT NULL,
  `country` varchar(255) DEFAULT NULL,
  `enable_refunds_in_dashboard` bit(1) DEFAULT NULL,
  `hdfc_merchant_code` varchar(255) DEFAULT NULL,
  `merchant_legal_name` varchar(255) DEFAULT NULL,
  `order_fields_in_settlement_report` varchar(255) DEFAULT NULL,
  `enable_sending_card_isin` bit(1) DEFAULT NULL,
  `ccavenue_account_id` varchar(255) DEFAULT NULL,
  `ccavenue_secret_key` varchar(255) DEFAULT NULL,
  `ccavenue_verified` bit(1) DEFAULT NULL,
  `mobile_version` varchar(255) DEFAULT NULL,
  `web_version` varchar(255) DEFAULT NULL,
  `iframe_version` varchar(255) DEFAULT NULL,
  `enable_order_notification` bit(1) DEFAULT NULL,
  `order_notification_email` varchar(255) DEFAULT NULL,
  `ccavenue_backend_gateway` varchar(255) DEFAULT NULL,
  `web_hookurl` varchar(255) DEFAULT NULL,
  `payu_test_mode` bit(1) DEFAULT NULL,
  `ebs_backend_gateway` varchar(255) DEFAULT NULL,
  `enable_automatic_retry` bit(1) DEFAULT NULL,
  `gateway_priority_logic` text,
  `use_code_for_gateway_priority` bit(1) NOT NULL DEFAULT b'0',
  `enable_storing_failed_card` bit(1) DEFAULT NULL,
  `enable_failed_card_storage` bit(1) DEFAULT NULL,
  `enable_save_card_before_auth` bit(1) DEFAULT NULL,
  `enable_save_card_before_authentication` bit(1) DEFAULT b'0',
  `auto_refund_conflict_transactions` bit(1) DEFAULT NULL,
  `payment_response_hash_key` varchar(255) DEFAULT NULL,
  `enable_payment_response_hash` bit(1) DEFAULT NULL,
  `enable_external_risk_check` bit(1) DEFAULT NULL,
  `citi_key` varchar(255) DEFAULT NULL,
  `citi_merchant_code` varchar(255) DEFAULT NULL,
  `citi_test_mode` bit(1) DEFAULT NULL,
  `citi_verified` bit(1) DEFAULT NULL,
  `conflict_status_email` varchar(255) DEFAULT NULL,
  `enable_conflict_status_notification` bit(1) DEFAULT NULL,
  `web_hook_password` varchar(255) DEFAULT NULL,
  `web_hook_username` varchar(255) DEFAULT NULL,
  `web_hookapiversion` varchar(255) DEFAULT NULL,
  `fetch_cards_from_payu` bit(1) DEFAULT NULL,
  `prefix_merchant_id_for_card_key` bit(1) DEFAULT NULL,
  `txn_id_custom_prefix` varchar(10) DEFAULT NULL,
  `secondary_merchant_account_id` bigint(20) DEFAULT NULL,
  `reseller_id1` varchar(64) DEFAULT NULL,
  `fingerprint_on_tokenize` bit(1) DEFAULT NULL,
  `card_encoding_key` varchar(255) DEFAULT NULL,
  `enable_unauthenticated_order_status_api` bit(1) DEFAULT NULL,
  `enable_unauthenticated_card_add` bit(1) DEFAULT NULL,
  `capture_authorizing_response` bit(1) DEFAULT NULL,
  `enable_unauthenticated_card_add2` bit(1) DEFAULT NULL,
  `process_authorizing_response` bit(1) DEFAULT NULL,
  `webhook_custom_headers` longtext,
  `timezone` varchar(255) DEFAULT NULL,
  `mandatory_2FA` tinyint(1) DEFAULT NULL,
  `order_session_timeout` tinyint(1) DEFAULT NULL,
  `include_surcharge_amount_for_refund` bit(1) DEFAULT NULL,
  `should_add_surcharge` bit(1) DEFAULT NULL,
  `show_surcharge_breakup_screen` bit(1) DEFAULT NULL,
  `reseller_id` varchar(32) DEFAULT NULL,
  `enable_api_white` bit(1) DEFAULT b'0',
  `enable_txn_filter` bit(1) DEFAULT b'0',
  `enabled_instant_refund` bit(1) DEFAULT NULL,
  `enable_payment_page_config` bit(1) DEFAULT b'0',
  PRIMARY KEY (`id`),
  KEY `FK47E2B2B67727E594` (`settlement_account_id`),
  KEY `FK47E2B2B6EB94360B` (`secondary_merchant_account_id`)
) ENGINE=InnoDB AUTO_INCREMENT=102 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `merchant_account`
--

LOCK TABLES `merchant_account` WRITE;
/*!40000 ALTER TABLE `merchant_account` DISABLE KEYS */;
INSERT INTO `merchant_account` VALUES (101,32,NULL,0,'juspay_test','Merchant Inc.',NULL,_binary '\0',_binary '\0',NULL,447,NULL,NULL,NULL,NULL,'',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,'m0010','2017-05-05 18:58:28','2017-05-05 18:58:27',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,_binary '\0',_binary '\0',_binary '\0',_binary '\0',NULL,NULL,_binary '',NULL,NULL,NULL,_binary '\0',NULL,NULL,_binary '\0',_binary '\0',NULL,_binary '','PAYU',NULL,NULL,_binary '',_binary '\0',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,_binary '',NULL,'Merchant Inc.',NULL,_binary '',NULL,NULL,NULL,'3_0',NULL,NULL,NULL,NULL,NULL,NULL,_binary '\0',NULL,_binary '\0','if (order.udf1 == \"yes\"){\r\n   Thread.sleep(30000)\r\n}\r\nsetGatewayPriority([\"RAZORPAY\"])',_binary '\0',NULL,NULL,_binary '\0',_binary '\0',NULL,'412CE161423A4252BF8A82027FF503E4',_binary '',_binary '\0',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,_binary '\0','7B77898D41CF4E399550ABFC5EDA1F5D',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,_binary '\0',_binary '\0',NULL,_binary '\0');
/*!40000 ALTER TABLE `merchant_account` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `merchant_iframe_preferences`
--

DROP TABLE IF EXISTS `merchant_iframe_preferences`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `merchant_iframe_preferences` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `version` bigint(20) NOT NULL,
  `custom_stylesheet` varchar(255) DEFAULT NULL,
  `merchant_id` varchar(255) NOT NULL,
  `return_url` varchar(255) DEFAULT NULL,
  `heading_text` varchar(255) DEFAULT NULL,
  `payment_button_label` varchar(255) DEFAULT NULL,
  `order_creation_url` varchar(255) DEFAULT NULL,
  `merchant_logo_url` varchar(255) DEFAULT NULL,
  `show_cancel_button` bit(1) DEFAULT NULL,
  `dynamic_switching_enabled` bit(1) DEFAULT NULL,
  `isin_routing_enabled` bit(1) DEFAULT NULL,
  `issuer_routing_enabled` bit(1) DEFAULT NULL,
  `mirror_gateway_response` bit(1) DEFAULT NULL,
  `save_card_enabled` bit(1) DEFAULT NULL,
  `txn_failure_gateway_penality` bit(1) DEFAULT NULL,
  `emergency_message` varchar(255) DEFAULT NULL,
  `form_show_amount` bit(1) DEFAULT NULL,
  `pass_through_mode` bit(1) DEFAULT NULL,
  `redirect_mode_only` bit(1) DEFAULT NULL,
  `delete_card_on_expiry` bit(1) DEFAULT NULL,
  `default_currency` varchar(255) DEFAULT NULL,
  `can_accept_amex` bit(1) DEFAULT NULL,
  `can_accept_diners` bit(1) DEFAULT NULL,
  `can_accept_discover` bit(1) DEFAULT NULL,
  `can_accept_jcb` bit(1) DEFAULT NULL,
  `can_accept_master` bit(1) DEFAULT NULL,
  `can_accept_rupay` bit(1) DEFAULT NULL,
  `can_accept_visa` bit(1) DEFAULT NULL,
  `can_accept_maestro` bit(1) DEFAULT NULL,
  `custom_mobile_stylesheet` varchar(255) DEFAULT NULL,
  `terms_and_conditions_link` text,
  `order_session_timeout` int(11) DEFAULT NULL,
  `card_brand_routing_enabled` bit(1) DEFAULT NULL,
  `always_accept_cards` bit(1) DEFAULT NULL,
  `wallet_topup_return_url` varchar(255) DEFAULT NULL,
  `additional_params_in_response` longtext,
  `additional_params_in_response1` longtext,
  `enable_web_payment_page` bit(1) DEFAULT NULL,
  `payment_page_upload_config_path` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `merchant_id` (`merchant_id`)
) ENGINE=InnoDB AUTO_INCREMENT=86 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `merchant_iframe_preferences`
--

LOCK TABLES `merchant_iframe_preferences` WRITE;
/*!40000 ALTER TABLE `merchant_iframe_preferences` DISABLE KEYS */;
INSERT INTO `merchant_iframe_preferences` VALUES (85,5,NULL,'juspay_test',NULL,NULL,NULL,NULL,NULL,NULL,NULL,_binary '',_binary '',NULL,_binary '',_binary '',NULL,NULL,NULL,_binary '',_binary '',NULL,_binary '\0',_binary '\0',_binary '\0',_binary '\0',NULL,_binary '\0',NULL,_binary '\0',NULL,NULL,200000,NULL,NULL,NULL,'{\"params\":[\"udf1\"]}',NULL,NULL,NULL);
/*!40000 ALTER TABLE `merchant_iframe_preferences` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `merchant_key`
--

DROP TABLE IF EXISTS `merchant_key`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `merchant_key` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `version` bigint(20) NOT NULL,
  `api_key` text DEFAULT NULL,
  `status` varchar(255) DEFAULT NULL,
  `date_created` datetime DEFAULT NULL,
  `last_updated` datetime DEFAULT NULL,
  `merchant_account_id` bigint(20) DEFAULT NULL,
  `scope` varchar(255) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK2504736837947956` (`merchant_account_id`)
) ENGINE=InnoDB AUTO_INCREMENT=151 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `merchant_key`
--

LOCK TABLES `merchant_key` WRITE;
/*!40000 ALTER TABLE `merchant_key` DISABLE KEYS */;
INSERT INTO `merchant_key` VALUES (150,0,'A3CB28E251414007963051A3729EFAC0','ACTIVE','2017-05-05 18:58:28','2017-05-05 18:58:28',101,'MERCHANT');
/*!40000 ALTER TABLE `merchant_key` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `order_address`
--

DROP TABLE IF EXISTS `order_address`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `order_address` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `version` bigint(20) NOT NULL,
  `city` varchar(255) DEFAULT NULL,
  `country` varchar(255) DEFAULT NULL,
  `country_code_iso` varchar(255) DEFAULT NULL,
  `first_name` varchar(255) DEFAULT NULL,
  `last_name` varchar(255) DEFAULT NULL,
  `line1` varchar(255) DEFAULT NULL,
  `line2` varchar(255) DEFAULT NULL,
  `line3` varchar(255) DEFAULT NULL,
  `phone` varchar(255) DEFAULT NULL,
  `postal_code` varchar(255) DEFAULT NULL,
  `state` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `order_address_id_index` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `order_address`
--

LOCK TABLES `order_address` WRITE;
/*!40000 ALTER TABLE `order_address` DISABLE KEYS */;
/*!40000 ALTER TABLE `order_address` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `order_metadata_v2`
--

DROP TABLE IF EXISTS `order_metadata_v2`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `order_metadata_v2` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `browser` varchar(255) DEFAULT NULL,
  `browser_version` varchar(255) DEFAULT NULL,
  `date_created` datetime NOT NULL,
  `device` varchar(255) DEFAULT NULL,
  `last_updated` date DEFAULT NULL,
  `metadata` longtext,
  `mobile` bit(1) DEFAULT NULL,
  `operating_system` varchar(255) DEFAULT NULL,
  `order_reference_id` bigint(20) NOT NULL,
  `ip_address` varchar(255) DEFAULT NULL,
  `referer` varchar(255) DEFAULT NULL,
  `user_agent` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `order_reference_id` (`order_reference_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `order_metadata_v2`
--

LOCK TABLES `order_metadata_v2` WRITE;
/*!40000 ALTER TABLE `order_metadata_v2` DISABLE KEYS */;
/*!40000 ALTER TABLE `order_metadata_v2` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `order_reference`
--

DROP TABLE IF EXISTS `order_reference`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `order_reference` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `version` bigint(20) NOT NULL,
  `amount` double NOT NULL,
  `currency` varchar(8) DEFAULT NULL,
  `date_created` datetime NOT NULL,
  `last_modified` datetime NOT NULL,
  `merchant_id` varchar(64) DEFAULT NULL,
  `order_id` varchar(64) DEFAULT NULL,
  `status` varchar(32) DEFAULT NULL,
  `customer_email` varchar(255) DEFAULT NULL,
  `customer_id` varchar(128) DEFAULT NULL,
  `browser` varchar(16) DEFAULT NULL,
  `browser_version` varchar(16) DEFAULT NULL,
  `popup_loaded` bit(1) DEFAULT NULL,
  `popup_loaded_time` datetime DEFAULT NULL,
  `description` varchar(255) DEFAULT NULL,
  `udf1` varchar(255) DEFAULT NULL,
  `udf10` varchar(255) DEFAULT NULL,
  `udf2` varchar(255) DEFAULT NULL,
  `udf3` varchar(255) DEFAULT NULL,
  `udf4` varchar(255) DEFAULT NULL,
  `udf5` varchar(255) DEFAULT NULL,
  `udf6` varchar(255) DEFAULT NULL,
  `udf7` varchar(255) DEFAULT NULL,
  `udf8` varchar(255) DEFAULT NULL,
  `udf9` varchar(255) DEFAULT NULL,
  `udf11` varchar(255) DEFAULT NULL,
  `return_url` varchar(255) DEFAULT NULL,
  `amount_refunded` double DEFAULT '0',
  `refunded_entirely` bit(1) DEFAULT b'0',
  `preferred_gateway` varchar(32) DEFAULT NULL,
  `customer_mobile` varchar(255) DEFAULT NULL,
  `customer_phone` varchar(255) DEFAULT NULL,
  `item_id` varchar(255) DEFAULT NULL,
  `product_id` varchar(255) DEFAULT NULL,
  `billing_address_id` bigint(20) DEFAULT NULL,
  `shipping_address_id` bigint(20) DEFAULT NULL,
  `auto_capture` bit(1) DEFAULT NULL,
  `order_uuid` varchar(255) DEFAULT NULL,
  `last_synced` datetime DEFAULT NULL,
  `order_type` varchar(255) DEFAULT NULL,
  `mandate_feature` varchar(255) DEFAULT NULL,
  `auto_refund` tinyint(1) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `FK7D1F779AC7415D90` (`shipping_address_id`),
  KEY `FK7D1F779ADA9706A3` (`billing_address_id`),
  KEY `amount` (`amount`)
) ENGINE=InnoDB AUTO_INCREMENT=9525 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `order_reference`
--

LOCK TABLES `order_reference` WRITE;
/*!40000 ALTER TABLE `order_reference` DISABLE KEYS */;
INSERT INTO `order_reference` VALUES (9524,1,10.4,'INR','2018-10-25 12:58:26','2018-10-25 12:58:26','juspay_test','Juspay_493','SUCCESS','mohana.ph@juspay.in','cst_zlcryqe9ih7xi2b5',NULL,NULL,NULL,NULL,'Sample description',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,'\"juspay\"','\"mohan\"',NULL,'https://google.co.in',0,_binary '\0','OLAPOSTPAID',NULL,'8123715658',NULL,'123',586,587,NULL,'ordeu_9a0bbcd66da546d7a98bf628b63160ec',NULL,'ORDER_PAYMENT','DISABLED',NULL);
/*!40000 ALTER TABLE `order_reference` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `service_configuration`
--

DROP TABLE IF EXISTS `service_configuration`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `service_configuration` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `version` bigint(20) NOT NULL,
  `name` varchar(255) NOT NULL,
  `value` varchar(512) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=59 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `service_configuration`
--

LOCK TABLES `service_configuration` WRITE;
/*!40000 ALTER TABLE `service_configuration` DISABLE KEYS */;
INSERT INTO `service_configuration` VALUES (1,0,'REPORT_DOWNLOAD_MAX_RESULTS','670'),(2,0,'GATEWAY_TXN_FAILURE_PENALITY','0.5'),(3,0,'PG_SC_CONNECT_TIMEOUT_PAYU','3000'),(4,0,'PG_SC_CONNECT_TIMEOUT_BILLDESK','3000'),(5,0,'PG_SC_READ_TIMEOUT_BILLDESK','3000'),(7,0,'PG_SC_READ_TIMEOUT_PAYU','3000'),(8,0,'PG_SHORT_CIRCUIT_PAYU_BACKOFF','300000'),(9,0,'PG_SHORT_CIRCUIT_BILLDESK_BACKOFF','300000'),(11,0,'PAYMENT_RESPONSE_HASH_ALGORITHM','HmacSHA512'),(12,0,'REFUND_QUEUE_EXECUTION_DELAY_SECONDS','30'),(13,0,'REFUND_QUEUE_MAX_ITERATIONS','2'),(14,0,'PAYU_REFUND_REITERATIONS_SECONDS','300'),(15,0,'PAYU_DUPLICATE_REFUND_ERROR_CODE','214'),(16,0,'PAYJSV2','1'),(25,0,'CITRUS_RECHECK_ERROR_MSG','[\"You did press back or refresh button.. Kindly don\'t perform such activity :), Citrus will take care of your payment. Kindly be paytience !!!\",\"Duplicate Merchant payment request\",\"Paid transaction exists\"]'),(26,0,'DUMMY_NB_PASSWORD','nb1221'),(27,0,'CITRUS_TXNS_READTIMEOUT_MILLISEC','40000'),(29,0,'RELOAD_SERVICE_CONFIG_TIMEOUT_MILLISEC','120000'),(34,0,'MAX_REFUND_REQUEST','50'),(35,0,'DIRECT_WALLET_TOKEN_EXPIRY_IN_SECOND','300'),(36,0,'RAZORPAY_TXNIDS','razorpay_payment_id,id'),(37,0,'MOBIKWIK_POWER_WALLET_TOKEN_GENERATION_AMOUNT','9000'),(38,0,'MOBIKWIK_POWER_WALLET_MAX_AMOUNT','5000'),(39,0,'CITRUS_TXNS_STATUS_READTIMEOUT_MILLISEC','5000'),(40,0,'TEMP_CARD_MAX_RETAIN_SECONDS','300'),(41,0,'PAYU_AUTHENTICATION_ERROR_CODES','[\"Bank failed to authenticate the customer due to 3D Secure Enrollment decline\",\"Card authentication failure\",\"3-D secure authentication failed.\",\"Bank failed to authenticate the customer due to 3D Secure Enrollment decline\",\"Card authentication failed at the bank due to invalid CVV (or CVC or Card Security Code)\",\"Authentication failure or there is a delay in processing the transaction.\",\"Bank was unable to authenticate.\",\"Transaction failed due to customer pressing cancel button.\"]'),(42,0,'TXN_SYNC_DELAY_IN_MILLI_SECS','800000'),(43,0,'GATEWAY_HEALTH_THRESHOLD_FOR_BAD','0.9'),(44,0,'GATEWAY_PARAMS_METADATA','[\'PAYTM\' : [\'CUST_ID\', \'PROMO_CAMP_ID\', \'COUPON_CODE\'], \'PAYU\'  : [\'offer_key\']]'),(45,0,'GATEWAY_CUSTOM_PARAMS_SPEC','{\"PAYTM\" : [\"CUST_ID\", \"PROMO_CAMP_ID\"], \"PAYU\" : [\"offer_key\"], \"OLAMONEY\" : [\"couponCode\"], \"CCAVENUE_V2\" : [\"promo_code\"], \"FREECHARGE\" : [\"campaignCode\"], \"TPSL\" : [\"shoppingCartDetails\", \"accountNo\"]}'),(46,0,'TXN_SYNC_DELAY_IN_MILLI_SECS','100'),(47,0,'RAZORPAY_ARN_ENABLED_MERCHANTS','[\"azharamin\"]'),(49,0,'BANK_ISSUER_NAME_MAPPING','{\"AXIS\" : [\"AXISBANK\", \"AXIS LTD\", \"AXIS\"], \"HDFC\" : [\"HDFC LTD\", \"HDFC BANK\"]}'),(50,0,'PAYU_GET_CARD_INFO_JUSPAY_MERCHANT_ID','juspay_test'),(51,0,'PAYU_GET_CARD_INFO_MERCHANT_GATEWAY_ACCOUNT_ID','62'),(52,0,'BIN_INFO_PROVIDERS','[\"PAYU\",\"CITRUS\"]'),(53,0,'IOS_STRING_FOR_GATEKEEPER','iphone'),(56,0,'service_config','service_config-v'),(57,0,'config-status','config-status-value'),(58,0,'config-name','config-value');
/*!40000 ALTER TABLE `service_configuration` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2020-03-18 15:11:06
