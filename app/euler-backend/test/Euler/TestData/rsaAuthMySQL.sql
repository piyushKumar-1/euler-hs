-- MySQL dump 10.13  Distrib 5.7.29, for Linux (x86_64)
--
-- Host: mysql    Database: rsa
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
INSERT INTO `merchant_account` VALUES (1,32,NULL,0,'merchantId','Merchant Inc.',NULL,_binary '\0',_binary '\0',NULL,447,NULL,NULL,NULL,NULL,'',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,'m0010','2017-05-05 18:58:28','2017-05-05 18:58:27',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,_binary '\0',_binary '\0',_binary '\0',_binary '\0',NULL,NULL,_binary '',NULL,NULL,NULL,_binary '\0',NULL,NULL,_binary '\0',_binary '\0',NULL,_binary '','PAYU',NULL,NULL,_binary '',_binary '\0',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,_binary '',NULL,'Merchant Inc.',NULL,_binary '',NULL,NULL,NULL,'3_0',NULL,NULL,NULL,NULL,NULL,NULL,_binary '\0',NULL,_binary '\0','if (order.udf1 == \"yes\"){\r\n   Thread.sleep(30000)\r\n}\r\nsetGatewayPriority([\"RAZORPAY\"])',_binary '\0',NULL,NULL,_binary '\0',_binary '\0',NULL,'412CE161423A4252BF8A82027FF503E4',_binary '',_binary '\0',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,_binary '\0','7B77898D41CF4E399550ABFC5EDA1F5D',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,_binary '\0',_binary '\0',NULL,_binary '\0');
/*!40000 ALTER TABLE `merchant_account` ENABLE KEYS */;
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
  `api_key` text,
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
INSERT INTO `merchant_key` VALUES (1,1,'-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAtqHS/+o+2GzDRapJHVXA\nakbRN4EKduYCI8czYiH3uptEkwj0OrCSolNwJDZFlAvGW1dXw92Fb0qADOSv4wYC\nDUhlD5+AYqcTgPSRLHSEK0Nmsc6hmXtXRS9fVK4hsqP3vqU4Q2A8AQbWUpArz8Zw\nDaItIzO0ofa+IPHlDC3CSjoSsKX+m07b2ZJusFXfpxa9zS89Avowt/a3kSr904Ys\nDrHwNTo1YX/63KqGlMc3BzqCoE6RBpz8K7RSUHwUk/1zruQ5/eOYGdW/ok4tGgNj\npCFY+oee1lsUACx+JxqDYjpUvi0WeT1AL1Klg7yOb1AHdYPtROJDo+gLbTlVYRMa\nwQIDAQAB\n-----END PUBLIC KEY-----\n','ACTIVE','2014-01-10 14:28:17','2014-01-10 14:28:17',1,'CLIENT_ENCRYPTION');
/*!40000 ALTER TABLE `merchant_key` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2020-03-18 16:08:09
