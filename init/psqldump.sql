--
-- PostgreSQL database dump
--

-- Dumped from database version 11.1
-- Dumped by pg_dump version 11.1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: jp_hpcl; Type: SCHEMA; Schema: -; Owner: cloud
--

CREATE SCHEMA jp_hpcl;


ALTER SCHEMA jp_hpcl OWNER TO cloud;

--
-- Name: loyalty_schema; Type: SCHEMA; Schema: -; Owner: cloud
--

CREATE SCHEMA loyalty_schema;


ALTER SCHEMA loyalty_schema OWNER TO cloud;

--
-- Name: enum_ActivationCodes_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_ActivationCodes_status" AS ENUM (
    'ENABLED',
    'DISABLED',
    'USED'
);


ALTER TYPE jp_hpcl."enum_ActivationCodes_status" OWNER TO cloud;

--
-- Name: enum_Agencies_category; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Agencies_category" AS ENUM (
    'LPG',
    'FUEL'
);


ALTER TYPE jp_hpcl."enum_Agencies_category" OWNER TO cloud;

--
-- Name: enum_Agencies_onBoardingStatus; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Agencies_onBoardingStatus" AS ENUM (
    'APPROVED',
    'PENDING'
);


ALTER TYPE jp_hpcl."enum_Agencies_onBoardingStatus" OWNER TO cloud;

--
-- Name: enum_Agencies_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Agencies_status" AS ENUM (
    'ENABLED',
    'DISABLED',
    'PENDING'
);


ALTER TYPE jp_hpcl."enum_Agencies_status" OWNER TO cloud;

--
-- Name: enum_Blacklists_context; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Blacklists_context" AS ENUM (
    'MERCHANT',
    'JUSPAY',
    'RESELLER'
);


ALTER TYPE jp_hpcl."enum_Blacklists_context" OWNER TO cloud;

--
-- Name: enum_Blacklists_sourceType; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Blacklists_sourceType" AS ENUM (
    'CARD_FINGERPRINT',
    'CUSTOMER',
    'MOBILE_NUMBER'
);


ALTER TYPE jp_hpcl."enum_Blacklists_sourceType" OWNER TO cloud;

--
-- Name: enum_Emails_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Emails_status" AS ENUM (
    'ENABLED',
    'DISABLED'
);


ALTER TYPE jp_hpcl."enum_Emails_status" OWNER TO cloud;

--
-- Name: enum_Employees_channel; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Employees_channel" AS ENUM (
    'ANDROID',
    'IOS',
    'FEATURE'
);


ALTER TYPE jp_hpcl."enum_Employees_channel" OWNER TO cloud;

--
-- Name: enum_Employees_deviceType; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Employees_deviceType" AS ENUM (
    'SMART_PHONE',
    'FEATURE_PHONE'
);


ALTER TYPE jp_hpcl."enum_Employees_deviceType" OWNER TO cloud;

--
-- Name: enum_Employees_role; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Employees_role" AS ENUM (
    'O_SUPERADMIN',
    'A_SUPERADMIN',
    'A_MANAGER',
    'A_AGENT'
);


ALTER TYPE jp_hpcl."enum_Employees_role" OWNER TO cloud;

--
-- Name: enum_Employees_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Employees_status" AS ENUM (
    'ENABLED',
    'DISABLED'
);


ALTER TYPE jp_hpcl."enum_Employees_status" OWNER TO cloud;

--
-- Name: enum_Logins_channel; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Logins_channel" AS ENUM (
    'WEB',
    'ANDROID'
);


ALTER TYPE jp_hpcl."enum_Logins_channel" OWNER TO cloud;

--
-- Name: enum_Logins_event; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Logins_event" AS ENUM (
    'MOBILE_LOGIN',
    'WEB_LOGIN',
    'VERIFY_OTP',
    'UNKNOWN'
);


ALTER TYPE jp_hpcl."enum_Logins_event" OWNER TO cloud;

--
-- Name: enum_Logins_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Logins_status" AS ENUM (
    'SUCCESS',
    'FAILURE'
);


ALTER TYPE jp_hpcl."enum_Logins_status" OWNER TO cloud;

--
-- Name: enum_MerchantKeys_role; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_MerchantKeys_role" AS ENUM (
    'O_SUPERADMIN',
    'A_SUPERADMIN',
    'A_MANAGER',
    'A_AGENT'
);


ALTER TYPE jp_hpcl."enum_MerchantKeys_role" OWNER TO cloud;

--
-- Name: enum_MerchantKeys_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_MerchantKeys_status" AS ENUM (
    'ENABLED',
    'DISABLED'
);


ALTER TYPE jp_hpcl."enum_MerchantKeys_status" OWNER TO cloud;

--
-- Name: enum_MerchantKeys_type; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_MerchantKeys_type" AS ENUM (
    'API',
    'CRON',
    'CHECKSUM'
);


ALTER TYPE jp_hpcl."enum_MerchantKeys_type" OWNER TO cloud;

--
-- Name: enum_MobileNumbers_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_MobileNumbers_status" AS ENUM (
    'ENABLED',
    'DISABLED'
);


ALTER TYPE jp_hpcl."enum_MobileNumbers_status" OWNER TO cloud;

--
-- Name: enum_OfferRedemptions_paymentMethodType; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_OfferRedemptions_paymentMethodType" AS ENUM (
    'ATM_CARD',
    'CARD',
    'CASH',
    'CONSUMER_FINANCE',
    'NB',
    'REWARD',
    'UPI',
    'WALLET'
);


ALTER TYPE jp_hpcl."enum_OfferRedemptions_paymentMethodType" OWNER TO cloud;

--
-- Name: enum_OfferRedemptions_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_OfferRedemptions_status" AS ENUM (
    'AVAILED',
    'INITIATED',
    'INVALID',
    'FAILED',
    'REFUNDED'
);


ALTER TYPE jp_hpcl."enum_OfferRedemptions_status" OWNER TO cloud;

--
-- Name: enum_Offers_applicationMode; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Offers_applicationMode" AS ENUM (
    'CASHBACK',
    'VOUCHER',
    'REWARD_POINT',
    'DISCOUNT'
);


ALTER TYPE jp_hpcl."enum_Offers_applicationMode" OWNER TO cloud;

--
-- Name: enum_Offers_calculationMode; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Offers_calculationMode" AS ENUM (
    'PERCENTAGE',
    'VALUE'
);


ALTER TYPE jp_hpcl."enum_Offers_calculationMode" OWNER TO cloud;

--
-- Name: enum_Orders_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Orders_status" AS ENUM (
    'SUCCESS',
    'FAILURE',
    'PENDING',
    'EXPIRED',
    'DECLINED',
    'FULFILLED'
);


ALTER TYPE jp_hpcl."enum_Orders_status" OWNER TO cloud;

--
-- Name: enum_Organizations_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Organizations_status" AS ENUM (
    'ENABLED',
    'DISABLED'
);


ALTER TYPE jp_hpcl."enum_Organizations_status" OWNER TO cloud;

--
-- Name: enum_PSPAccounts_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_PSPAccounts_status" AS ENUM (
    'ENABLED',
    'DISABLED'
);


ALTER TYPE jp_hpcl."enum_PSPAccounts_status" OWNER TO cloud;

--
-- Name: enum_PSPResponses_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_PSPResponses_status" AS ENUM (
    'SUCCESS',
    'FAILURE',
    'PENDING',
    'EXPIRED',
    'DECLINED',
    'TIMED_OUT',
    'COLLECT_PENDING'
);


ALTER TYPE jp_hpcl."enum_PSPResponses_status" OWNER TO cloud;

--
-- Name: enum_PSPs_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_PSPs_status" AS ENUM (
    'ENABLED',
    'DISABLED'
);


ALTER TYPE jp_hpcl."enum_PSPs_status" OWNER TO cloud;

--
-- Name: enum_Processes_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Processes_status" AS ENUM (
    'STARTED',
    'WAITING_USER',
    'WAITING_SYSTEM',
    'COMPLETED',
    'SUSPENDED',
    'WAITING'
);


ALTER TYPE jp_hpcl."enum_Processes_status" OWNER TO cloud;

--
-- Name: enum_Refunds_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Refunds_status" AS ENUM (
    'INITIATED',
    'PENDING',
    'SUCCESS',
    'FAILURE'
);


ALTER TYPE jp_hpcl."enum_Refunds_status" OWNER TO cloud;

--
-- Name: enum_Rules_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Rules_status" AS ENUM (
    'APPROVED',
    'PENDING',
    'REJECTED',
    'DISABLED'
);


ALTER TYPE jp_hpcl."enum_Rules_status" OWNER TO cloud;

--
-- Name: enum_Rules_type; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Rules_type" AS ENUM (
    'SURCHARGE',
    'PRIORITY_LOGIC'
);


ALTER TYPE jp_hpcl."enum_Rules_type" OWNER TO cloud;

--
-- Name: enum_SavedPaymentMethods_paymentMethod; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_SavedPaymentMethods_paymentMethod" AS ENUM (
    'WALLET',
    'NB',
    'UPI_PAY',
    'UPI_COLLECT',
    'CARD',
    'ATM_CARD',
    'CONSUMER_FINANCE'
);


ALTER TYPE jp_hpcl."enum_SavedPaymentMethods_paymentMethod" OWNER TO cloud;

--
-- Name: enum_SettlementAccounts_source; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_SettlementAccounts_source" AS ENUM (
    'APP',
    'MANUAL'
);


ALTER TYPE jp_hpcl."enum_SettlementAccounts_source" OWNER TO cloud;

--
-- Name: enum_SettlementAccounts_verified; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_SettlementAccounts_verified" AS ENUM (
    'SUCCESS',
    'PENDING_VERIFICATION',
    'PENDING_DEPOSIT',
    'IMPS_SUCCESS',
    'FAILURE'
);


ALTER TYPE jp_hpcl."enum_SettlementAccounts_verified" OWNER TO cloud;

--
-- Name: enum_Settlements_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Settlements_status" AS ENUM (
    'PENDING',
    'SUCCESS',
    'CONFLICT'
);


ALTER TYPE jp_hpcl."enum_Settlements_status" OWNER TO cloud;

--
-- Name: enum_Settlements_type; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Settlements_type" AS ENUM (
    'PAY',
    'COLLECT',
    'REFUND'
);


ALTER TYPE jp_hpcl."enum_Settlements_type" OWNER TO cloud;

--
-- Name: enum_Shipments_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Shipments_status" AS ENUM (
    'PENDING',
    'IN_TRANSIT',
    'ON_HOLD',
    'RETURNED',
    'FAILED',
    'DELIVERED'
);


ALTER TYPE jp_hpcl."enum_Shipments_status" OWNER TO cloud;

--
-- Name: enum_StarterKits_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_StarterKits_status" AS ENUM (
    'ENABLED',
    'DISABLED'
);


ALTER TYPE jp_hpcl."enum_StarterKits_status" OWNER TO cloud;

--
-- Name: enum_StarterKits_trackingStatus; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_StarterKits_trackingStatus" AS ENUM (
    'GENERATED',
    'PRINTING',
    'PRINTED'
);


ALTER TYPE jp_hpcl."enum_StarterKits_trackingStatus" OWNER TO cloud;

--
-- Name: enum_StaticAssets_type; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_StaticAssets_type" AS ENUM (
    'BADGE',
    'A4_POSTER',
    'BANNER',
    'STANDEE',
    'STANDEE_KANNADA',
    'ACRYLIC',
    'LOGO',
    'OTHERS'
);


ALTER TYPE jp_hpcl."enum_StaticAssets_type" OWNER TO cloud;

--
-- Name: enum_Templates_channel; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Templates_channel" AS ENUM (
    'SMS',
    'EMAIL',
    'CALL',
    'GCM'
);


ALTER TYPE jp_hpcl."enum_Templates_channel" OWNER TO cloud;

--
-- Name: enum_Transactions_mode; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Transactions_mode" AS ENUM (
    'IFSC',
    'UPI',
    'BHARAT_QR',
    'PPI'
);


ALTER TYPE jp_hpcl."enum_Transactions_mode" OWNER TO cloud;

--
-- Name: enum_Transactions_qrSource; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Transactions_qrSource" AS ENUM (
    'ANDROID',
    'IOS',
    'WEB',
    'A4_POSTER',
    'STANDEE',
    'BADGE',
    'ACRYLIC',
    'DISPENSER'
);


ALTER TYPE jp_hpcl."enum_Transactions_qrSource" OWNER TO cloud;

--
-- Name: enum_Transactions_settlementStatus; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Transactions_settlementStatus" AS ENUM (
    'SETTLED',
    'PENDING',
    'FAILURE'
);


ALTER TYPE jp_hpcl."enum_Transactions_settlementStatus" OWNER TO cloud;

--
-- Name: enum_Transactions_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Transactions_status" AS ENUM (
    'SUCCESS',
    'FAILURE',
    'PENDING',
    'EXPIRED',
    'DECLINED',
    'TIMED_OUT',
    'COLLECT_PENDING'
);


ALTER TYPE jp_hpcl."enum_Transactions_status" OWNER TO cloud;

--
-- Name: enum_Transactions_transactionSource; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Transactions_transactionSource" AS ENUM (
    'APP',
    'MERCHANT',
    'SYNC',
    'BACKFILL',
    'HEALTH'
);


ALTER TYPE jp_hpcl."enum_Transactions_transactionSource" OWNER TO cloud;

--
-- Name: enum_Transactions_type; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Transactions_type" AS ENUM (
    'PAY',
    'COLLECT'
);


ALTER TYPE jp_hpcl."enum_Transactions_type" OWNER TO cloud;

--
-- Name: enum_TxnFilters_context; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_TxnFilters_context" AS ENUM (
    'MERCHANT',
    'JUSPAY',
    'RESELLER'
);


ALTER TYPE jp_hpcl."enum_TxnFilters_context" OWNER TO cloud;

--
-- Name: enum_TxnFilters_sourceType; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_TxnFilters_sourceType" AS ENUM (
    'CARD_FINGERPRINT',
    'CUSTOMER',
    'MOBILE_NUMBER',
    'CARD_ISIN'
);


ALTER TYPE jp_hpcl."enum_TxnFilters_sourceType" OWNER TO cloud;

--
-- Name: enum_TxnFilters_type; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_TxnFilters_type" AS ENUM (
    'BLACKLIST',
    'WHITELIST'
);


ALTER TYPE jp_hpcl."enum_TxnFilters_type" OWNER TO cloud;

--
-- Name: enum_Vpas_status; Type: TYPE; Schema: jp_hpcl; Owner: cloud
--

CREATE TYPE jp_hpcl."enum_Vpas_status" AS ENUM (
    'ENABLED',
    'DISABLED'
);


ALTER TYPE jp_hpcl."enum_Vpas_status" OWNER TO cloud;

--
-- Name: enum_LoyaltyCustomers_channel; Type: TYPE; Schema: loyalty_schema; Owner: cloud
--

CREATE TYPE loyalty_schema."enum_LoyaltyCustomers_channel" AS ENUM (
    'ANDROID',
    'IOS'
);


ALTER TYPE loyalty_schema."enum_LoyaltyCustomers_channel" OWNER TO cloud;

--
-- Name: enum_LoyaltyCustomers_language; Type: TYPE; Schema: loyalty_schema; Owner: cloud
--

CREATE TYPE loyalty_schema."enum_LoyaltyCustomers_language" AS ENUM (
    'ENGLISH'
);


ALTER TYPE loyalty_schema."enum_LoyaltyCustomers_language" OWNER TO cloud;

--
-- Name: enum_LoyaltyCustomers_status; Type: TYPE; Schema: loyalty_schema; Owner: cloud
--

CREATE TYPE loyalty_schema."enum_LoyaltyCustomers_status" AS ENUM (
    'ONBOARDED',
    'UNCLAIMED',
    'CLAIMED'
);


ALTER TYPE loyalty_schema."enum_LoyaltyCustomers_status" OWNER TO cloud;

--
-- Name: enum_LoyaltyStatics_source; Type: TYPE; Schema: loyalty_schema; Owner: cloud
--

CREATE TYPE loyalty_schema."enum_LoyaltyStatics_source" AS ENUM (
    'BHIM',
    'PHONEPE',
    'PAYTM',
    'TEZ'
);


ALTER TYPE loyalty_schema."enum_LoyaltyStatics_source" OWNER TO cloud;

--
-- Name: enum_LoyaltyStatics_type; Type: TYPE; Schema: loyalty_schema; Owner: cloud
--

CREATE TYPE loyalty_schema."enum_LoyaltyStatics_type" AS ENUM (
    'CASHBACK',
    'REWARD'
);


ALTER TYPE loyalty_schema."enum_LoyaltyStatics_type" OWNER TO cloud;

--
-- Name: enum_LoyaltyTokens_entityType; Type: TYPE; Schema: loyalty_schema; Owner: cloud
--

CREATE TYPE loyalty_schema."enum_LoyaltyTokens_entityType" AS ENUM (
    'CUSTOMER',
    'OTHER'
);


ALTER TYPE loyalty_schema."enum_LoyaltyTokens_entityType" OWNER TO cloud;

--
-- Name: enum_LoyaltyTransactions_mode; Type: TYPE; Schema: loyalty_schema; Owner: cloud
--

CREATE TYPE loyalty_schema."enum_LoyaltyTransactions_mode" AS ENUM (
    'UPI'
);


ALTER TYPE loyalty_schema."enum_LoyaltyTransactions_mode" OWNER TO cloud;

--
-- Name: enum_LoyaltyTransactions_rewardStatus; Type: TYPE; Schema: loyalty_schema; Owner: cloud
--

CREATE TYPE loyalty_schema."enum_LoyaltyTransactions_rewardStatus" AS ENUM (
    'CLAIMED',
    'UNCLAIMED'
);


ALTER TYPE loyalty_schema."enum_LoyaltyTransactions_rewardStatus" OWNER TO cloud;

--
-- Name: enum_LoyaltyTransactions_source; Type: TYPE; Schema: loyalty_schema; Owner: cloud
--

CREATE TYPE loyalty_schema."enum_LoyaltyTransactions_source" AS ENUM (
    'BHIM',
    'PHONEPE',
    'PAYTM',
    'TEZ'
);


ALTER TYPE loyalty_schema."enum_LoyaltyTransactions_source" OWNER TO cloud;

--
-- Name: enum_LoyaltyTransactions_status; Type: TYPE; Schema: loyalty_schema; Owner: cloud
--

CREATE TYPE loyalty_schema."enum_LoyaltyTransactions_status" AS ENUM (
    'SUCCESS',
    'FAILURE',
    'PENDING',
    'EXPIRED',
    'DECLINED',
    'TIMED_OUT',
    'COLLECT_PENDING'
);


ALTER TYPE loyalty_schema."enum_LoyaltyTransactions_status" OWNER TO cloud;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: ActivationCodes; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."ActivationCodes" (
    id character varying(255) NOT NULL,
    code character varying(255) NOT NULL,
    "EmployeeId" character varying(255),
    "AgencyId" character varying(255) NOT NULL,
    status jp_hpcl."enum_ActivationCodes_status" DEFAULT 'ENABLED'::jp_hpcl."enum_ActivationCodes_status" NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."ActivationCodes" OWNER TO cloud;

--
-- Name: ActivityLogs; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."ActivityLogs" (
    id character varying(255) NOT NULL,
    "shortName" character varying(255),
    description text NOT NULL,
    remarks character varying(255),
    "UserId" character varying(255),
    "ProcessId" character varying(255) NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."ActivityLogs" OWNER TO cloud;

--
-- Name: Addresses; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Addresses" (
    id character varying(255) NOT NULL,
    landmark character varying(255),
    address text NOT NULL,
    pincode character varying(255) NOT NULL,
    city character varying(255) NOT NULL,
    state character varying(255) NOT NULL,
    "StateId" character varying(255) NOT NULL,
    info json,
    country character varying(255) DEFAULT 'INDIA'::character varying NOT NULL,
    location character varying(255),
    district character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Addresses" OWNER TO cloud;

--
-- Name: Agencies; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Agencies" (
    id character varying(255) NOT NULL,
    name character varying(255) NOT NULL,
    code character varying(255) NOT NULL,
    "merchantCode" character varying(255) NOT NULL,
    status jp_hpcl."enum_Agencies_status" DEFAULT 'PENDING'::jp_hpcl."enum_Agencies_status" NOT NULL,
    category jp_hpcl."enum_Agencies_category",
    "onBoardingStatus" jp_hpcl."enum_Agencies_onBoardingStatus" DEFAULT 'PENDING'::jp_hpcl."enum_Agencies_onBoardingStatus" NOT NULL,
    "supportEmail" character varying(255),
    "supportContact" character varying(255),
    "AddressId" character varying(255),
    "OrganizationId" character varying(255) NOT NULL,
    info json,
    "notifyCustomerOnCollect" boolean DEFAULT false,
    "smsSender" character varying(255),
    "shouldSendTxnCallback" boolean DEFAULT false,
    "callbackUrl" character varying(255),
    "shouldSendTxnSMS" boolean DEFAULT true,
    "shouldSendTxnGCM" boolean DEFAULT true,
    zone character varying(255),
    region character varying(255),
    "salesArea" character varying(255),
    "modeOfOperation" character varying(255),
    "MerchantId" character varying(255),
    "MerchantAccountId" integer,
    "overrideCallbackUrl" boolean,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Agencies" OWNER TO cloud;

--
-- Name: Applications; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Applications" (
    id character varying(255) NOT NULL,
    "applicationName" character varying(255) NOT NULL,
    "AgencyId" character varying(255) NOT NULL,
    "authToken" character varying(255),
    "linkedOn" character varying(255),
    status character varying(255) NOT NULL,
    "maxLimit" character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Applications" OWNER TO cloud;

--
-- Name: BankAccounts; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."BankAccounts" (
    id character varying(255) NOT NULL,
    "accountNumber" character varying(255) NOT NULL,
    name character varying(255) NOT NULL,
    ifsc character varying(255) NOT NULL,
    "visaPAN" character varying(255),
    "visaPAN2" character varying(255),
    "rupayPAN" character varying(255),
    "rupayPAN2" character varying(255),
    "masterPAN" character varying(255),
    "masterPAN2" character varying(255),
    "amexPAN" character varying(255),
    "amexPAN2" character varying(255),
    branch character varying(255),
    "PSPId" character varying(255) NOT NULL,
    "AgencyId" character varying(255) NOT NULL,
    info json,
    "terminalID" character varying(255) NOT NULL,
    "PspAccountId" character varying(255) NOT NULL,
    "mobileNumber" character varying(255) NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."BankAccounts" OWNER TO cloud;

--
-- Name: Blacklists; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Blacklists" (
    id character varying(255) NOT NULL,
    "OwnerId" character varying(255) NOT NULL,
    active boolean,
    context jp_hpcl."enum_Blacklists_context" DEFAULT 'MERCHANT'::jp_hpcl."enum_Blacklists_context" NOT NULL,
    "sourceType" jp_hpcl."enum_Blacklists_sourceType" DEFAULT 'CARD_FINGERPRINT'::jp_hpcl."enum_Blacklists_sourceType" NOT NULL,
    value character varying(255) NOT NULL,
    metadata json,
    "blacklistReason" character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Blacklists" OWNER TO cloud;

--
-- Name: Configurations; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Configurations" (
    id character varying(255) NOT NULL,
    key character varying(255) NOT NULL,
    "referenceId" character varying(255),
    value json,
    "referenceType" character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Configurations" OWNER TO cloud;

--
-- Name: CustomData; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."CustomData" (
    id character varying(255) NOT NULL,
    "AgencyId" character varying(255) NOT NULL,
    "OrganizationId" character varying(255) NOT NULL,
    "AnchorEntityId" character varying(255) NOT NULL,
    "anchorEntityName" character varying(255) NOT NULL,
    "metaData" json NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."CustomData" OWNER TO cloud;

--
-- Name: Customers; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Customers" (
    id character varying(255) NOT NULL,
    vpa character varying(255) NOT NULL,
    "normalizedVpa" character varying(255) NOT NULL,
    name character varying(255),
    "mobileNumber" character varying(255),
    info json,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Customers" OWNER TO cloud;

--
-- Name: Devices; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Devices" (
    id character varying(255) NOT NULL,
    os character varying(255),
    "deviceId" character varying(255) NOT NULL,
    model character varying(255),
    "packageName" character varying(255),
    version character varying(255),
    manufacturer character varying(255),
    info json,
    "EmployeeId" character varying(255),
    "MobileNumberId" character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Devices" OWNER TO cloud;

--
-- Name: Emails; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Emails" (
    id character varying(255) NOT NULL,
    email character varying(255) NOT NULL,
    password character varying(255),
    "EmployeeId" character varying(255),
    status jp_hpcl."enum_Emails_status" DEFAULT 'ENABLED'::jp_hpcl."enum_Emails_status" NOT NULL,
    verified boolean DEFAULT false,
    salt character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Emails" OWNER TO cloud;

--
-- Name: Employees; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Employees" (
    id character varying(255) NOT NULL,
    code character varying(255) NOT NULL,
    name character varying(255),
    language character varying(255),
    channel jp_hpcl."enum_Employees_channel",
    role jp_hpcl."enum_Employees_role" NOT NULL,
    info json,
    "invitedBy" character varying(255),
    "AgencyId" character varying(255) NOT NULL,
    status jp_hpcl."enum_Employees_status" DEFAULT 'ENABLED'::jp_hpcl."enum_Employees_status" NOT NULL,
    "deviceType" jp_hpcl."enum_Employees_deviceType",
    "shouldSendTxnSMS" boolean DEFAULT false,
    "shouldSendTxnGCM" boolean DEFAULT false,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Employees" OWNER TO cloud;

--
-- Name: EntityTags; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."EntityTags" (
    id character varying(255) NOT NULL,
    "EntityId" character varying(255) NOT NULL,
    "entityName" character varying(255) NOT NULL,
    "TagId" character varying(255) NOT NULL,
    "TaggedBy" character varying(255) NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."EntityTags" OWNER TO cloud;

--
-- Name: Events; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Events" (
    id character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    label character varying(255) NOT NULL,
    value character varying(255) NOT NULL,
    info json,
    udf1 character varying(255),
    udf2 character varying(255),
    "CreatedBy" character varying(255) NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Events" OWNER TO cloud;

--
-- Name: ExternalMerchantCustomers; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."ExternalMerchantCustomers" (
    id character varying(255) NOT NULL,
    "MerchantId" character varying(255) NOT NULL,
    "CustomerId" character varying(255) NOT NULL,
    "legacyCustomerId" character varying(255) NOT NULL,
    "OrganizationId" character varying(255) NOT NULL,
    info json,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."ExternalMerchantCustomers" OWNER TO cloud;

--
-- Name: Logins; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Logins" (
    id character varying(255) NOT NULL,
    channel jp_hpcl."enum_Logins_channel" NOT NULL,
    "ipAddress" character varying(255),
    "EmployeeId" character varying(255) NOT NULL,
    "userAgent" character varying(255),
    host character varying(255),
    event jp_hpcl."enum_Logins_event" DEFAULT 'UNKNOWN'::jp_hpcl."enum_Logins_event" NOT NULL,
    status jp_hpcl."enum_Logins_status" NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Logins" OWNER TO cloud;

--
-- Name: MerchantKeys; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."MerchantKeys" (
    id character varying(255) NOT NULL,
    "accessId" character varying(255),
    "secretKey" character varying(255) NOT NULL,
    role jp_hpcl."enum_MerchantKeys_role",
    type jp_hpcl."enum_MerchantKeys_type",
    "AgencyId" character varying(255),
    "OrganizationId" character varying(255) NOT NULL,
    status jp_hpcl."enum_MerchantKeys_status" DEFAULT 'ENABLED'::jp_hpcl."enum_MerchantKeys_status" NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."MerchantKeys" OWNER TO cloud;

--
-- Name: MobileNumbers; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."MobileNumbers" (
    id character varying(255) NOT NULL,
    "mobileNumber" character varying(255) NOT NULL,
    "gcmId" character varying(255),
    "passcodeHash" character varying(255),
    "appVersion" character varying(255),
    "configVersion" character varying(255),
    "bundleVersion" character varying(255),
    "EmployeeId" character varying(255),
    status jp_hpcl."enum_MobileNumbers_status" DEFAULT 'ENABLED'::jp_hpcl."enum_MobileNumbers_status" NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."MobileNumbers" OWNER TO cloud;

--
-- Name: OfferRedemptions; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."OfferRedemptions" (
    id character varying(255) NOT NULL,
    "CustomerId" character varying(255),
    "AgencyId" character varying(255) NOT NULL,
    "OfferId" character varying(255) NOT NULL,
    "OrderId" character varying(255) NOT NULL,
    status jp_hpcl."enum_OfferRedemptions_status" NOT NULL,
    "txnId" character varying(255),
    "txnUuid" character varying(255),
    "originalAmount" double precision,
    "discountedAmount" double precision,
    "discountValue" double precision,
    "paymentMethodType" jp_hpcl."enum_OfferRedemptions_paymentMethodType",
    "paymentInstrument" character varying(255),
    udf1 character varying(255),
    udf2 character varying(255),
    udf3 character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."OfferRedemptions" OWNER TO cloud;

--
-- Name: Offers; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Offers" (
    id character varying(255) NOT NULL,
    "AgencyId" character varying(255) NOT NULL,
    "OrganizationId" character varying(255) NOT NULL,
    active boolean DEFAULT false NOT NULL,
    "voucherCode" character varying(255) NOT NULL,
    "gatewayCode" character varying(255),
    "offerDescription" json,
    "offerValidityStart" timestamp with time zone NOT NULL,
    "offerValidityEnd" timestamp with time zone NOT NULL,
    "offerStart" time without time zone,
    "offerEnd" time without time zone,
    "applicableDay" integer[] DEFAULT ARRAY[]::integer[],
    "minimumCartValue" double precision DEFAULT 1,
    "maximumCartValue" double precision,
    "applicationMode" jp_hpcl."enum_Offers_applicationMode" NOT NULL,
    "calculationMode" jp_hpcl."enum_Offers_calculationMode" NOT NULL,
    "discountValue" double precision NOT NULL,
    "maxAggregateAmount" double precision,
    "paymentPartnerShare" double precision,
    sponsor character varying(255)[],
    "maximumDiscount" double precision,
    budget double precision,
    "paymentMethodType" character varying(255),
    "paymentMethod" character varying(255)[],
    "paymentMethodFilter" character varying(255)[],
    "allowedRedemption" integer[] DEFAULT ARRAY[0],
    "gatewayOverride" character varying(255),
    "repeatableRedemption" double precision,
    "paymentChannel" character varying(255)[],
    "visibleToCustomer" boolean DEFAULT false,
    "exclusiveOffer" json,
    "proceedOnError" boolean DEFAULT false,
    udf1 character varying(255),
    udf10 character varying(255),
    udf2 character varying(255),
    udf3 character varying(255),
    udf4 character varying(255),
    udf5 character varying(255),
    udf6 character varying(255),
    udf7 character varying(255),
    udf8 character varying(255),
    udf9 character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Offers" OWNER TO cloud;

--
-- Name: Orders; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Orders" (
    id character varying(255) NOT NULL,
    "AgencyId" character varying(255) NOT NULL,
    "merchantOrderId" character varying(255),
    "ExternalMerchantCustomerId" character varying(255) NOT NULL,
    amount numeric(10,2) NOT NULL,
    currency character varying(255) DEFAULT 'INR'::character varying NOT NULL,
    status jp_hpcl."enum_Orders_status" NOT NULL,
    info json,
    udf1 character varying(255),
    udf2 character varying(255),
    udf3 character varying(255),
    udf4 character varying(255),
    udf5 character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Orders" OWNER TO cloud;

--
-- Name: Organizations; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Organizations" (
    id character varying(255) NOT NULL,
    name character varying(255) NOT NULL,
    code character varying(255) NOT NULL,
    handle character varying(255) NOT NULL,
    email character varying(255),
    "supportEmail" character varying(255),
    "supportContact" character varying(255),
    status jp_hpcl."enum_Organizations_status" DEFAULT 'ENABLED'::jp_hpcl."enum_Organizations_status" NOT NULL,
    info json,
    "AddressId" character varying(255) NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Organizations" OWNER TO cloud;

--
-- Name: PSPAccounts; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."PSPAccounts" (
    id character varying(255) NOT NULL,
    "PSPId" character varying(255) NOT NULL,
    "AgencyId" character varying(255) NOT NULL,
    status jp_hpcl."enum_PSPAccounts_status" NOT NULL,
    creds json,
    "isPrimary" boolean NOT NULL,
    info json,
    "merchantReferenceCode" character varying(255),
    percentage numeric(10,2),
    "apiConfig" json,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."PSPAccounts" OWNER TO cloud;

--
-- Name: PSPResponses; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."PSPResponses" (
    id character varying(255) NOT NULL,
    "TransactionId" character varying(255),
    payload text,
    "transactionObject" text,
    status jp_hpcl."enum_PSPResponses_status",
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."PSPResponses" OWNER TO cloud;

--
-- Name: PSPs; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."PSPs" (
    id character varying(255) NOT NULL,
    "shortCode" character varying(255) NOT NULL,
    handle character varying(255) NOT NULL,
    "vpaHandle" character varying(255) NOT NULL,
    "vpaFormat" character varying(255) NOT NULL,
    name character varying(255) NOT NULL,
    status jp_hpcl."enum_PSPs_status" NOT NULL,
    "supportEmail" character varying(255),
    "supportContact" character varying(255),
    "apiInfo" json,
    info json,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."PSPs" OWNER TO cloud;

--
-- Name: Pincodes; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Pincodes" (
    id character varying(255) NOT NULL,
    pincode character varying(255) NOT NULL,
    "ShipmentDealerIds" character varying(255)[] NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Pincodes" OWNER TO cloud;

--
-- Name: PingChecks; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."PingChecks" (
    id character varying(255) NOT NULL,
    label character varying(255) NOT NULL,
    info json,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."PingChecks" OWNER TO cloud;

--
-- Name: ProcessTracker; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."ProcessTracker" (
    id character varying(255) NOT NULL,
    name character varying(255),
    tag text[] NOT NULL,
    runner character varying(255),
    "retryCount" integer NOT NULL,
    "scheduleTime" timestamp with time zone,
    rule character varying(255) NOT NULL,
    "trackingData" json NOT NULL,
    "businessStatus" character varying(255) NOT NULL,
    status character varying(255) NOT NULL,
    event text[] NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."ProcessTracker" OWNER TO cloud;

--
-- Name: Processes; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Processes" (
    id character varying(255) NOT NULL,
    "shortName" character varying(255),
    name character varying(255) NOT NULL,
    "AnchorEntityId" character varying(255),
    "anchorEntityName" character varying(255),
    "anchorEntityInfo" character varying(255),
    state json,
    status jp_hpcl."enum_Processes_status" DEFAULT 'STARTED'::jp_hpcl."enum_Processes_status" NOT NULL,
    "OwnerId" character varying(255),
    "ParentId" character varying(255),
    deadline timestamp with time zone,
    "metaData" json,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Processes" OWNER TO cloud;

--
-- Name: Refunds; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Refunds" (
    id character varying(255) NOT NULL,
    "AgencyId" character varying(255) NOT NULL,
    "OrderId" character varying(255),
    "TransactionId" character varying(255) NOT NULL,
    "refundDate" timestamp with time zone DEFAULT timezone('utc'::text, now()) NOT NULL,
    "refundRefId" character varying(255) NOT NULL,
    "transactionRef" character varying(255),
    status jp_hpcl."enum_Refunds_status" NOT NULL,
    amount numeric(10,2) NOT NULL,
    "responseCode" character varying(255),
    info json,
    "customerInfo" json,
    udf1 character varying(255),
    udf2 character varying(255),
    udf3 character varying(255),
    udf4 character varying(255),
    udf5 character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Refunds" OWNER TO cloud;

--
-- Name: RegistrationTokens; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."RegistrationTokens" (
    id character varying(255) NOT NULL,
    "EmployeeId" character varying(255),
    "DeviceId" character varying(255),
    token character varying(255) NOT NULL,
    verified boolean DEFAULT false NOT NULL,
    "expiresIn" bigint NOT NULL,
    "smsDetails" json,
    "smsContent" character varying(255) DEFAULT ''::character varying NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."RegistrationTokens" OWNER TO cloud;

--
-- Name: Rules; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Rules" (
    id character varying(255) NOT NULL,
    "MerchantAccountId" character varying(255) NOT NULL,
    markdown character varying(255),
    rule text NOT NULL,
    status jp_hpcl."enum_Rules_status" DEFAULT 'PENDING'::jp_hpcl."enum_Rules_status" NOT NULL,
    active boolean DEFAULT true NOT NULL,
    type jp_hpcl."enum_Rules_type" DEFAULT 'SURCHARGE'::jp_hpcl."enum_Rules_type" NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Rules" OWNER TO cloud;

--
-- Name: SavedPaymentMethods; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."SavedPaymentMethods" (
    id character varying(255) NOT NULL,
    "MerchantId" character varying(255) NOT NULL,
    "OrganizationId" character varying(255) NOT NULL,
    "ExternalMerchantCustomerId" character varying(255) NOT NULL,
    "paymentMethod" jp_hpcl."enum_SavedPaymentMethods_paymentMethod" NOT NULL,
    value character varying(255) NOT NULL,
    count integer NOT NULL,
    "paymentMetadata" json,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."SavedPaymentMethods" OWNER TO cloud;

--
-- Name: SettlementAccounts; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."SettlementAccounts" (
    id character varying(255) NOT NULL,
    "AgencyId" character varying(255) NOT NULL,
    "VpaId" character varying(255) NOT NULL,
    "beneficiaryName" character varying(255) NOT NULL,
    "beneficiaryBankName" character varying(255),
    ifsc character varying(255) NOT NULL,
    "branchName" character varying(255),
    "beneficiaryAccountNumber" character varying(255) NOT NULL,
    "isPrimary" boolean NOT NULL,
    source jp_hpcl."enum_SettlementAccounts_source",
    verified jp_hpcl."enum_SettlementAccounts_verified" DEFAULT 'PENDING_DEPOSIT'::jp_hpcl."enum_SettlementAccounts_verified" NOT NULL,
    info json,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."SettlementAccounts" OWNER TO cloud;

--
-- Name: SettlementBatches; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."SettlementBatches" (
    id character varying(255) NOT NULL,
    "SettlementAccountId" character varying(255) NOT NULL,
    "AgencyId" character varying(255),
    date timestamp with time zone NOT NULL,
    amount numeric(10,2) NOT NULL,
    info json,
    "remitterAccountNumber" character varying(255) NOT NULL,
    narration character varying(255) NOT NULL,
    "utrNo" character varying(255) NOT NULL,
    mode character varying(255) NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."SettlementBatches" OWNER TO cloud;

--
-- Name: SettlementLogs; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."SettlementLogs" (
    id character varying(255) NOT NULL,
    "PSPId" character varying(255) NOT NULL,
    "batchDate" timestamp with time zone NOT NULL,
    "batchAmount" numeric(10,2) NOT NULL,
    info json,
    duration integer,
    hostname character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."SettlementLogs" OWNER TO cloud;

--
-- Name: Settlements; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Settlements" (
    id character varying(255) NOT NULL,
    "SettlementBatchId" character varying(255) NOT NULL,
    "TransactionId" character varying(255) NOT NULL,
    "CustomerId" character varying(255) NOT NULL,
    "AgencyId" character varying(255) NOT NULL,
    date timestamp with time zone NOT NULL,
    "payerVpa" character varying(255),
    "payeeVpa" character varying(255),
    "grossTxnAmount" numeric(10,2) NOT NULL,
    commission numeric(10,2),
    tax numeric(10,2),
    "netAmount" numeric(10,2) NOT NULL,
    type jp_hpcl."enum_Settlements_type" NOT NULL,
    status jp_hpcl."enum_Settlements_status" NOT NULL,
    udf1 character varying(255),
    udf2 character varying(255),
    udf3 character varying(255),
    udf4 character varying(255),
    udf5 character varying(255),
    "transactionAt" timestamp with time zone,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Settlements" OWNER TO cloud;

--
-- Name: ShipmentDealers; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."ShipmentDealers" (
    id character varying(255) NOT NULL,
    info json,
    name character varying(255) NOT NULL,
    handle character varying(255) NOT NULL,
    shortcode character varying(255) NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."ShipmentDealers" OWNER TO cloud;

--
-- Name: Shipments; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Shipments" (
    id character varying(255) NOT NULL,
    "AgencyId" character varying(255) NOT NULL,
    status jp_hpcl."enum_Shipments_status" DEFAULT 'PENDING'::jp_hpcl."enum_Shipments_status" NOT NULL,
    info json,
    "waybillNumber" character varying(255),
    "StarterKitId" character varying(255),
    "ShipmentDealerId" character varying(255) NOT NULL,
    "shipmentLabelUrl" character varying(255),
    remarks character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Shipments" OWNER TO cloud;

--
-- Name: StarterKits; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."StarterKits" (
    id character varying(255) NOT NULL,
    code character varying(255),
    "formFactors" json NOT NULL,
    path character varying(255) NOT NULL,
    "AgencyId" character varying(255) NOT NULL,
    "trackingStatus" jp_hpcl."enum_StarterKits_trackingStatus" DEFAULT 'GENERATED'::jp_hpcl."enum_StarterKits_trackingStatus" NOT NULL,
    info json,
    version character varying(255),
    "StateId" character varying(255),
    "ShipmentDealerId" character varying(255),
    status jp_hpcl."enum_StarterKits_status" DEFAULT 'ENABLED'::jp_hpcl."enum_StarterKits_status" NOT NULL,
    "PSPId" character varying(255) NOT NULL,
    "CreatedBy" character varying(255) NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."StarterKits" OWNER TO cloud;

--
-- Name: States; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."States" (
    id character varying(255) NOT NULL,
    name character varying(255) NOT NULL,
    "shortCode" character varying(255) NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."States" OWNER TO cloud;

--
-- Name: StaticAssets; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."StaticAssets" (
    id character varying(255) NOT NULL,
    name character varying(255),
    url character varying(255),
    object text,
    type jp_hpcl."enum_StaticAssets_type" NOT NULL,
    dimensions json,
    code character varying(255),
    "PSPId" character varying(255),
    "OrganizationId" character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."StaticAssets" OWNER TO cloud;

--
-- Name: Tags; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Tags" (
    id character varying(255) NOT NULL,
    name character varying(255) NOT NULL,
    "CreatedBy" character varying(255) NOT NULL,
    description character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Tags" OWNER TO cloud;

--
-- Name: Templates; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Templates" (
    id character varying(255) NOT NULL,
    name character varying(255) NOT NULL,
    content json NOT NULL,
    channel jp_hpcl."enum_Templates_channel" NOT NULL,
    "CreatedBy" character varying(255) NOT NULL,
    description character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Templates" OWNER TO cloud;

--
-- Name: TransactionReplications; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."TransactionReplications" (
    id character varying(255) NOT NULL,
    "AgencyId" character varying(255) NOT NULL,
    "ExternalMerchantCustomerId" character varying(255) NOT NULL,
    "orderId" character varying(255) NOT NULL,
    "txnId" character varying(255),
    "txnUuid" character varying(255),
    amount double precision,
    "customerEmail" character varying(255),
    "customerPhone" character varying(255),
    gateway character varying(255),
    "cardIsin" character varying(6),
    "cardLastFourDigits" character varying(4),
    "nameOnCard" character varying(255),
    "paymentMethodType" character varying(255),
    "paymentMethod" character varying(255),
    udf1 character varying(255),
    udf2 character varying(255),
    udf3 character varying(255),
    udf4 character varying(255),
    udf5 character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."TransactionReplications" OWNER TO cloud;

--
-- Name: Transactions; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Transactions" (
    id character varying(255) NOT NULL,
    "AgencyId" character varying(255) NOT NULL,
    "OrderId" character varying(255) NOT NULL,
    "transactionRef" character varying(255),
    "PayeeVpaId" character varying(255) NOT NULL,
    "payerVpa" character varying(255),
    "payeeVpa" character varying(255),
    "payerInfo" json,
    "payeeInfo" json,
    info json,
    "selfInitiated" boolean,
    mode jp_hpcl."enum_Transactions_mode" DEFAULT 'UPI'::jp_hpcl."enum_Transactions_mode" NOT NULL,
    amount numeric(10,2) NOT NULL,
    "upiRequestId" character varying(255),
    "BharatQRTxnId" character varying(255),
    type jp_hpcl."enum_Transactions_type" NOT NULL,
    status jp_hpcl."enum_Transactions_status" NOT NULL,
    "upiMsgId" character varying(255),
    "npciResponse" json,
    remarks character varying(255) NOT NULL,
    expiry timestamp with time zone,
    "transactionAt" timestamp with time zone DEFAULT timezone('utc'::text, now()),
    currency character varying(255) DEFAULT 'INR'::character varying NOT NULL,
    "custRef" character varying(255),
    "refUrl" character varying(255),
    "transactionSource" jp_hpcl."enum_Transactions_transactionSource",
    "qrSource" jp_hpcl."enum_Transactions_qrSource",
    "settlementStatus" jp_hpcl."enum_Transactions_settlementStatus",
    "CustomerId" character varying(255),
    "EmployeeId" character varying(255),
    "bankName" character varying(255),
    "accountNumber" character varying(255),
    "bankIFSC" character varying(255),
    udf1 character varying(255),
    udf2 character varying(255),
    udf3 character varying(255),
    udf4 character varying(255),
    udf5 character varying(255),
    "callbackUrl" character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Transactions" OWNER TO cloud;

--
-- Name: TxnFilters; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."TxnFilters" (
    id character varying(255) NOT NULL,
    "OwnerId" character varying(255) NOT NULL,
    active boolean,
    context jp_hpcl."enum_TxnFilters_context" DEFAULT 'MERCHANT'::jp_hpcl."enum_TxnFilters_context" NOT NULL,
    "sourceType" jp_hpcl."enum_TxnFilters_sourceType" DEFAULT 'CARD_FINGERPRINT'::jp_hpcl."enum_TxnFilters_sourceType" NOT NULL,
    type jp_hpcl."enum_TxnFilters_type" DEFAULT 'WHITELIST'::jp_hpcl."enum_TxnFilters_type" NOT NULL,
    value character varying(255) NOT NULL,
    metadata json,
    "filterReason" character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."TxnFilters" OWNER TO cloud;

--
-- Name: Vpas; Type: TABLE; Schema: jp_hpcl; Owner: cloud
--

CREATE TABLE jp_hpcl."Vpas" (
    id character varying(255) NOT NULL,
    "PSPId" character varying(255) NOT NULL,
    "PspAccountId" character varying(255) NOT NULL,
    vpa character varying(255) NOT NULL,
    "normalizedVpa" character varying(255) NOT NULL,
    "AgencyId" character varying(255) NOT NULL,
    "CustomerId" character varying(255),
    status jp_hpcl."enum_Vpas_status" DEFAULT 'ENABLED'::jp_hpcl."enum_Vpas_status" NOT NULL,
    "isPrimary" boolean NOT NULL,
    info json,
    "sendNotifications" boolean DEFAULT false,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE jp_hpcl."Vpas" OWNER TO cloud;

--
-- Name: LoyaltyCustomers; Type: TABLE; Schema: loyalty_schema; Owner: cloud
--

CREATE TABLE loyalty_schema."LoyaltyCustomers" (
    id character varying(255) NOT NULL,
    "verifoneId" character varying(255),
    "deviceInfo" json,
    status loyalty_schema."enum_LoyaltyCustomers_status" DEFAULT 'UNCLAIMED'::loyalty_schema."enum_LoyaltyCustomers_status",
    "mobileNumber" character varying(255),
    "gcmId" character varying(255),
    language loyalty_schema."enum_LoyaltyCustomers_language",
    channel loyalty_schema."enum_LoyaltyCustomers_channel",
    version character varying(255),
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE loyalty_schema."LoyaltyCustomers" OWNER TO cloud;

--
-- Name: LoyaltyStatics; Type: TABLE; Schema: loyalty_schema; Owner: cloud
--

CREATE TABLE loyalty_schema."LoyaltyStatics" (
    id character varying(255) NOT NULL,
    key character varying(255) NOT NULL,
    type loyalty_schema."enum_LoyaltyStatics_type",
    source loyalty_schema."enum_LoyaltyStatics_source",
    value character varying(255) NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE loyalty_schema."LoyaltyStatics" OWNER TO cloud;

--
-- Name: LoyaltyTokens; Type: TABLE; Schema: loyalty_schema; Owner: cloud
--

CREATE TABLE loyalty_schema."LoyaltyTokens" (
    id character varying(255) NOT NULL,
    token character varying(255),
    verified boolean DEFAULT false NOT NULL,
    "expiresIn" bigint NOT NULL,
    "EntityId" character varying(255),
    "entityType" loyalty_schema."enum_LoyaltyTokens_entityType",
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE loyalty_schema."LoyaltyTokens" OWNER TO cloud;

--
-- Name: LoyaltyTransactions; Type: TABLE; Schema: loyalty_schema; Owner: cloud
--

CREATE TABLE loyalty_schema."LoyaltyTransactions" (
    id character varying(255) NOT NULL,
    "AgencyId" character varying(255) NOT NULL,
    "payerVpa" character varying(255) NOT NULL,
    amount numeric(10,2) NOT NULL,
    "payeeVpa" character varying(255) NOT NULL,
    "payeeName" character varying(255),
    source loyalty_schema."enum_LoyaltyTransactions_source",
    status loyalty_schema."enum_LoyaltyTransactions_status",
    info json,
    mode loyalty_schema."enum_LoyaltyTransactions_mode",
    cashback numeric(10,2) NOT NULL,
    reward numeric(10,2) NOT NULL,
    "rewardStatus" loyalty_schema."enum_LoyaltyTransactions_rewardStatus",
    "custRef" character varying(255) NOT NULL,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE loyalty_schema."LoyaltyTransactions" OWNER TO cloud;

--
-- Name: LoyaltyVPAs; Type: TABLE; Schema: loyalty_schema; Owner: cloud
--

CREATE TABLE loyalty_schema."LoyaltyVPAs" (
    id character varying(255) NOT NULL,
    "CustomerId" character varying(255),
    vpa character varying(255) NOT NULL,
    "isVerified" boolean DEFAULT false,
    "isPrimary" boolean DEFAULT false,
    "createdAt" timestamp with time zone NOT NULL,
    "updatedAt" timestamp with time zone NOT NULL
);


ALTER TABLE loyalty_schema."LoyaltyVPAs" OWNER TO cloud;

--
-- Data for Name: ActivationCodes; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."ActivationCodes" (id, code, "EmployeeId", "AgencyId", status, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: ActivityLogs; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."ActivityLogs" (id, "shortName", description, remarks, "UserId", "ProcessId", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Addresses; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Addresses" (id, landmark, address, pincode, city, state, "StateId", info, country, location, district, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Agencies; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Agencies" (id, name, code, "merchantCode", status, category, "onBoardingStatus", "supportEmail", "supportContact", "AddressId", "OrganizationId", info, "notifyCustomerOnCollect", "smsSender", "shouldSendTxnCallback", "callbackUrl", "shouldSendTxnSMS", "shouldSendTxnGCM", zone, region, "salesArea", "modeOfOperation", "MerchantId", "MerchantAccountId", "overrideCallbackUrl", "createdAt", "updatedAt") FROM stdin;
A47a02a00fd0411783e001f2e37e96e1	JUSPAY TEST MID	0001	ECAXS0001	ENABLED	\N	APPROVED	\N	919087705230	A2eb115a89ef4267a7856dd76a84375	Aa3258f7dc7e457eb5b81c5765281a3	\N	f	\N	t	https://postman-echo.com/post	t	t	\N	\N	\N	\N	\N	\N	\N	2018-02-23 12:27:56.930761+05:30	2018-02-23 12:27:56.930761+05:30
Afc7766dafa748a9944b96a9451518b	shubham_juspay-2	40	40	PENDING	\N	PENDING	\N	\N	\N	primaryKey001	\N	f	\N	f	\N	t	t	\N	\N	\N	\N	shubham_juspay-2	40	\N	2018-11-21 14:06:29+05:30	2018-11-21 14:06:29+05:30
A597e84955b5465daa5c7f1cb41eeeb	shubham_juspay-3	41	41	PENDING	\N	PENDING	\N	\N	\N	primaryKey001	\N	f	\N	f	\N	t	t	\N	\N	\N	\N	shubham_juspay-3	41	\N	2018-11-21 14:06:39+05:30	2018-11-21 14:06:39+05:30
Abffd3496017447288d381f2008ab6b	shubham_juspay-4	42	42	PENDING	\N	PENDING	\N	\N	\N	primaryKey001	\N	f	\N	f	\N	t	t	\N	\N	\N	\N	shubham_juspay-4	42	\N	2018-11-21 14:08:11+05:30	2018-11-21 14:08:11+05:30
Af56e33ee4b84298b8dfe9c55b2d480	shubham_juspay-5	43	43	PENDING	\N	PENDING	\N	\N	\N	primaryKey001	\N	f	\N	f	\N	t	t	\N	\N	\N	\N	shubham_juspay-5	43	\N	2018-11-21 14:10:18+05:30	2018-11-21 14:10:18+05:30
Ab753faf79cd44399269e77ae3d6eef	anuj_juspay	107	107	PENDING	\N	PENDING	\N	\N	\N	primaryKey001	\N	f	\N	f	\N	t	t	\N	\N	\N	\N	anuj_juspay	107	\N	2018-12-21 14:51:41+05:30	2018-12-21 14:51:41+05:30
Abc4657417a1435eb3e950dc4875518	anuj	108	108	PENDING	\N	PENDING	\N	\N	\N	primaryKey001	\N	f	\N	f	\N	t	t	\N	\N	\N	\N	anuj	108	\N	2018-12-21 14:54:50+05:30	2018-12-21 14:54:50+05:30
A4eaea23160d431199b825e9bcf47ab	anuj_switch	109	109	PENDING	\N	PENDING	\N	\N	\N	primaryKey001	\N	f	\N	f	\N	t	t	\N	\N	\N	\N	anuj_switch	109	\N	2019-01-21 17:10:15+05:30	2019-01-21 17:10:15+05:30
Abd79a42c5bd425bb2c29033f12f7f9	pawan_juspay	103	103	PENDING	\N	PENDING	\N	\N	\N	primaryKey001	\N	f	\N	f	\N	t	t	\N	\N	\N	\N	pawan_juspay	103	\N	2019-02-01 17:48:39+05:30	2019-02-01 17:48:39+05:30
\.


--
-- Data for Name: Applications; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Applications" (id, "applicationName", "AgencyId", "authToken", "linkedOn", status, "maxLimit", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: BankAccounts; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."BankAccounts" (id, "accountNumber", name, ifsc, "visaPAN", "visaPAN2", "rupayPAN", "rupayPAN2", "masterPAN", "masterPAN2", "amexPAN", "amexPAN2", branch, "PSPId", "AgencyId", info, "terminalID", "PspAccountId", "mobileNumber", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Blacklists; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Blacklists" (id, "OwnerId", active, context, "sourceType", value, metadata, "blacklistReason", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Configurations; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Configurations" (id, key, "referenceId", value, "referenceType", "createdAt", "updatedAt") FROM stdin;
771affca69064a96b99e2dc53db98894	RAZORPAY_CLIENT_SECRET_UAT	razorpay_oauth_dashboard_config	"AQICAHhCRqudVJvCKJkgXroHwPTm5Js+dlXptS7f+WPhBBWUrAG+qq15t/qqt1JDNM/pMSdNAAAAdjB0BgkqhkiG9w0BBwagZzBlAgEAMGAGCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMdGwmIYeGCcLtrnSpAgEQgDPpySHWZ/hFANBzYrgPz0W3tln/NK0OMdrpcISezCN0iXvsSHz+KJuV5NiNv9YY+a0MRpY="	dashboard_v3	2019-02-20 14:57:19+05:30	2019-02-20 14:57:19+05:30
771affca69we3a96b99e2dc53d8894	RAZORPAY_CLIENT_SECRET_UAT_UAT	razorpay_oauth_dashboard_config	"AQICAHhCRqudVJvCKJkgXroHwPTm5Js+dlXptS7f+WPhBBWUrAG+qq15t/qqt1JDNM/pMSdNAAAAdjB0BgkqhkiG9w0BBwagZzBlAgEAMGAGCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMdGwmIYeGCcLtrnSpAgEQgDPpySHWZ/hFANBzYrgPz0W3tln/NK0OMdrpcISezCN0iXvsSHz+KJuV5NiNv9YY+a0MRpY="	dashboard_v3	2019-03-06 18:04:06.750926+05:30	2019-03-06 18:04:06.750926+05:30
9ceb898cc03a471eb29d9286b0804ed7	razorpay_uat_creds_axis-bank	123	{"client_id": "232321", "client_secret": "232mek2m3m23m2om32m3"}	creds	2019-08-06 19:23:34.612334+05:30	2019-08-06 19:23:34.612334+05:30
9ceb898cc03a471eb29d9286b0804edp	razorpay_prod_creds_axis-bank	123	{"client_id": "232321", "client_secret": "232mek2m3m23m2om32m3"}	creds	2019-08-08 14:47:21.803224+05:30	2019-08-08 14:47:21.803224+05:30
\.


--
-- Data for Name: CustomData; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."CustomData" (id, "AgencyId", "OrganizationId", "AnchorEntityId", "anchorEntityName", "metaData", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Customers; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Customers" (id, vpa, "normalizedVpa", name, "mobileNumber", info, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Devices; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Devices" (id, os, "deviceId", model, "packageName", version, manufacturer, info, "EmployeeId", "MobileNumberId", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Emails; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Emails" (id, email, password, "EmployeeId", status, verified, salt, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Employees; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Employees" (id, code, name, language, channel, role, info, "invitedBy", "AgencyId", status, "deviceType", "shouldSendTxnSMS", "shouldSendTxnGCM", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: EntityTags; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."EntityTags" (id, "EntityId", "entityName", "TagId", "TaggedBy", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Events; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Events" (id, category, label, value, info, udf1, udf2, "CreatedBy", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: ExternalMerchantCustomers; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."ExternalMerchantCustomers" (id, "MerchantId", "CustomerId", "legacyCustomerId", "OrganizationId", info, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Logins; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Logins" (id, channel, "ipAddress", "EmployeeId", "userAgent", host, event, status, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: MerchantKeys; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."MerchantKeys" (id, "accessId", "secretKey", role, type, "AgencyId", "OrganizationId", status, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: MobileNumbers; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."MobileNumbers" (id, "mobileNumber", "gcmId", "passcodeHash", "appVersion", "configVersion", "bundleVersion", "EmployeeId", status, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: OfferRedemptions; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."OfferRedemptions" (id, "CustomerId", "AgencyId", "OfferId", "OrderId", status, "txnId", "txnUuid", "originalAmount", "discountedAmount", "discountValue", "paymentMethodType", "paymentInstrument", udf1, udf2, udf3, "createdAt", "updatedAt") FROM stdin;
b6e218c6161465ebfb4966aa82adbc	\N	A2cec2be5a0f34ceaac2e841e6423c4b8	1baae21eed748bca1657efe649d87c	ord1_7nyPQFw3bY2lIVVL	INITIATED	anuj_juspay-ord1_7nyPQFw3bY2lIVVL-1	ugzynd3rae4xuj8k	499	499	399	CARD		\N	\N	\N	2018-10-31 12:46:28+05:30	2018-10-31 12:46:28.447+05:30
7a08cb68b8c43829e6700969a30f4b	\N	A2cec2be5a0f34ceaac2e841e6423c4b8	fed4d005ca641118e6dd7dead2a997	ord1_7nyPQFw3bY2lIVVL	INITIATED	anuj_juspay-ord1_7nyPQFw3bY2lIVVL-1	ugzynd3rae4xuj8k	499	100	399	CARD		\N	\N	\N	2018-10-31 12:47:27+05:30	2018-10-31 12:47:27.59+05:30
3aa72680a454e7e94d586dffc86cfc	\N	A2cec2be5a0f34ceaac2e841e6423c4b8	fed4d005ca641118e6dd7dead2a997	ord1_7nyPQFw3bY2lIVVL	INITIATED	anuj_juspay-ord1_7nyPQFw3bY2lIVVL-1	ugzynd3rae4xuj8k	499	399	100	CARD		\N	\N	\N	2018-10-31 12:49:15+05:30	2018-10-31 12:49:15.356+05:30
16b4b3400ce491ab684aaf8f6939f8	\N	A2cec2be5a0f34ceaac2e841e6423c4b8	1baae21eed748bca1657efe649d87c	ord1_7nyPQFw3bY2lIVVL	INITIATED	anuj_juspay-ord1_7nyPQFw3bY2lIVVL-1	ugzynd3rae4xuj8k	499	499	100	CARD		\N	\N	\N	2018-10-31 12:49:29+05:30	2018-10-31 12:49:29.396+05:30
\.


--
-- Data for Name: Offers; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Offers" (id, "AgencyId", "OrganizationId", active, "voucherCode", "gatewayCode", "offerDescription", "offerValidityStart", "offerValidityEnd", "offerStart", "offerEnd", "applicableDay", "minimumCartValue", "maximumCartValue", "applicationMode", "calculationMode", "discountValue", "maxAggregateAmount", "paymentPartnerShare", sponsor, "maximumDiscount", budget, "paymentMethodType", "paymentMethod", "paymentMethodFilter", "allowedRedemption", "gatewayOverride", "repeatableRedemption", "paymentChannel", "visibleToCustomer", "exclusiveOffer", "proceedOnError", udf1, udf10, udf2, udf3, udf4, udf5, udf6, udf7, udf8, udf9, "createdAt", "updatedAt") FROM stdin;
1baae21eed748bca1657efe649d87c	A2cec2be5a0f34ceaac2e841e6423c4b8	Ad290f4c4ad0a47fc87ae5a19388d45bb	t	TEST_PAYTM	\N	{"offerDisplay1":"Terms and conditions apply.","offerDescription":"Flat 10% CASHBACK on all orders."}	2018-06-01 05:30:00+05:30	2019-06-01 05:30:00+05:30	\N	\N	{1,2,3}	125	\N	CASHBACK	VALUE	100	\N	\N	{}	500	\N	CARD	{AMEX,DINERS,DISCOVER,MAESTRO,MASTERCARD,RUPAY,VISA}	{424242::424242}	{0}	\N	\N	{WEB,MOBILE_WEB,ANDROID,IOS}	t	{"productCategory":[],"location":[],"customerId":[]}	t	\N	CARD_BIN	\N	\N	\N	\N	\N	\N	\N	\N	2018-10-31 12:43:31+05:30	2018-10-31 12:43:32.38+05:30
7c15bf0d4dc4821881adf896f2785d	Ab753faf79cd44399269e77ae3d6eef	primaryKey001	t	TEST_PAYTM	\N	{"offerDisplay1":"Terms and conditions apply.","offerDescription":"Flat 10% CASHBACK on all orders."}	2018-06-01 05:30:00+05:30	2019-06-01 05:30:00+05:30	\N	\N	{1,2,3}	1	\N	DISCOUNT	PERCENTAGE	10	\N	\N	{}	500	\N	WALLET	{PAYTM}	\N	{}	\N	\N	\N	t	{"productCategory":[],"location":[],"customerId":[]}	t	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	2019-01-02 13:35:14+05:30	2019-01-02 13:35:14.615+05:30
\.


--
-- Data for Name: Orders; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Orders" (id, "AgencyId", "merchantOrderId", "ExternalMerchantCustomerId", amount, currency, status, info, udf1, udf2, udf3, udf4, udf5, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Organizations; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Organizations" (id, name, code, handle, email, "supportEmail", "supportContact", status, info, "AddressId", "createdAt", "updatedAt") FROM stdin;
primaryKey001	EC	EC00001	EC	anuj.chandra@juspay.in	\N	918123220993	ENABLED	\N	AddressId	2018-11-21 14:06:21.433665+05:30	2018-11-21 14:06:21.433665+05:30
\.


--
-- Data for Name: PSPAccounts; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."PSPAccounts" (id, "PSPId", "AgencyId", status, creds, "isPrimary", info, "merchantReferenceCode", percentage, "apiConfig", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: PSPResponses; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."PSPResponses" (id, "TransactionId", payload, "transactionObject", status, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: PSPs; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."PSPs" (id, "shortCode", handle, "vpaHandle", "vpaFormat", name, status, "supportEmail", "supportContact", "apiInfo", info, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Pincodes; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Pincodes" (id, pincode, "ShipmentDealerIds", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: PingChecks; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."PingChecks" (id, label, info, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: ProcessTracker; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."ProcessTracker" (id, name, tag, runner, "retryCount", "scheduleTime", rule, "trackingData", "businessStatus", status, event, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Processes; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Processes" (id, "shortName", name, "AnchorEntityId", "anchorEntityName", "anchorEntityInfo", state, status, "OwnerId", "ParentId", deadline, "metaData", "createdAt", "updatedAt") FROM stdin;
82b2d98faf14d04862c4a01ed7763b	BULK_CUSTOMER_CARD_DELETE	Delete card for Customer abc	Abd79a42c5bd425bb2c29033f12f7f9	Agencies	\N	\N	SUSPENDED	abc	10dd635b8c945a4bd0b76e51a95f5a	\N	{"status":"{\\"status\\":\\"Invalid Authentication\\",\\"error_code\\":\\"access_denied\\",\\"error_message\\":\\"Invalid Authentication\\"}"}	2019-02-01 17:58:01+05:30	2019-02-01 17:58:01+05:30
0f9efeceadd4a27aa9da277658583e	BULK_CUSTOMER_CARD_DELETE	Delete card for Customer def	Abd79a42c5bd425bb2c29033f12f7f9	Agencies	\N	\N	SUSPENDED	def	10dd635b8c945a4bd0b76e51a95f5a	\N	{"status":"{\\"status\\":\\"Invalid Authentication\\",\\"error_code\\":\\"access_denied\\",\\"error_message\\":\\"Invalid Authentication\\"}"}	2019-02-01 17:58:01+05:30	2019-02-01 17:58:01+05:30
4b03055e88844c3be537e1f8987e70	BULK_CUSTOMER_CARD_DELETE	Delete card for Customer ghi	Abd79a42c5bd425bb2c29033f12f7f9	Agencies	\N	\N	SUSPENDED	ghi	10dd635b8c945a4bd0b76e51a95f5a	\N	{"status":"{\\"status\\":\\"Invalid Authentication\\",\\"error_code\\":\\"access_denied\\",\\"error_message\\":\\"Invalid Authentication\\"}"}	2019-02-01 17:58:01+05:30	2019-02-01 17:58:01+05:30
279d613d81242408451c07b539e996	BULK_CUSTOMER_CARD_DELETE	Delete card for Customer dfc	Abd79a42c5bd425bb2c29033f12f7f9	Agencies	\N	\N	SUSPENDED	dfc	10dd635b8c945a4bd0b76e51a95f5a	\N	{"status":"{\\"status\\":\\"Invalid Authentication\\",\\"error_code\\":\\"access_denied\\",\\"error_message\\":\\"Invalid Authentication\\"}"}	2019-02-01 17:58:01+05:30	2019-02-01 17:58:01+05:30
e5f45fec1634d35b74f917a61e3e39	BULK_CUSTOMER_CARD_DELETE	Delete card for Customer djsb	Abd79a42c5bd425bb2c29033f12f7f9	Agencies	\N	\N	SUSPENDED	djsb	10dd635b8c945a4bd0b76e51a95f5a	\N	{"status":"{\\"status\\":\\"Invalid Authentication\\",\\"error_code\\":\\"access_denied\\",\\"error_message\\":\\"Invalid Authentication\\"}"}	2019-02-01 17:58:01+05:30	2019-02-01 17:58:01+05:30
b1fac002e6f44dfb192439137989b0	BULK_CUSTOMER_CARD_DELETE	Delete card for Customer dsbjk	Abd79a42c5bd425bb2c29033f12f7f9	Agencies	\N	\N	SUSPENDED	dsbjk	10dd635b8c945a4bd0b76e51a95f5a	\N	{"status":"{\\"status\\":\\"Invalid Authentication\\",\\"error_code\\":\\"access_denied\\",\\"error_message\\":\\"Invalid Authentication\\"}"}	2019-02-01 17:58:01+05:30	2019-02-01 17:58:01+05:30
10dd635b8c945a4bd0b76e51a95f5a	BULK_CUSTOMER_CARD_DELETE	Delete card for Customer	Abd79a42c5bd425bb2c29033f12f7f9	Agencies	\N	\N	COMPLETED	\N	\N	\N	{"fileName":"ext.txt"}	2019-02-01 17:58:01+05:30	2019-02-01 17:58:01+05:30
\.


--
-- Data for Name: Refunds; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Refunds" (id, "AgencyId", "OrderId", "TransactionId", "refundDate", "refundRefId", "transactionRef", status, amount, "responseCode", info, "customerInfo", udf1, udf2, udf3, udf4, udf5, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: RegistrationTokens; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."RegistrationTokens" (id, "EmployeeId", "DeviceId", token, verified, "expiresIn", "smsDetails", "smsContent", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Rules; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Rules" (id, "MerchantAccountId", markdown, rule, status, active, type, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: SavedPaymentMethods; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."SavedPaymentMethods" (id, "MerchantId", "OrganizationId", "ExternalMerchantCustomerId", "paymentMethod", value, count, "paymentMetadata", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: SettlementAccounts; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."SettlementAccounts" (id, "AgencyId", "VpaId", "beneficiaryName", "beneficiaryBankName", ifsc, "branchName", "beneficiaryAccountNumber", "isPrimary", source, verified, info, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: SettlementBatches; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."SettlementBatches" (id, "SettlementAccountId", "AgencyId", date, amount, info, "remitterAccountNumber", narration, "utrNo", mode, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: SettlementLogs; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."SettlementLogs" (id, "PSPId", "batchDate", "batchAmount", info, duration, hostname, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Settlements; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Settlements" (id, "SettlementBatchId", "TransactionId", "CustomerId", "AgencyId", date, "payerVpa", "payeeVpa", "grossTxnAmount", commission, tax, "netAmount", type, status, udf1, udf2, udf3, udf4, udf5, "transactionAt", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: ShipmentDealers; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."ShipmentDealers" (id, info, name, handle, shortcode, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Shipments; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Shipments" (id, "AgencyId", status, info, "waybillNumber", "StarterKitId", "ShipmentDealerId", "shipmentLabelUrl", remarks, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: StarterKits; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."StarterKits" (id, code, "formFactors", path, "AgencyId", "trackingStatus", info, version, "StateId", "ShipmentDealerId", status, "PSPId", "CreatedBy", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: States; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."States" (id, name, "shortCode", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: StaticAssets; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."StaticAssets" (id, name, url, object, type, dimensions, code, "PSPId", "OrganizationId", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Tags; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Tags" (id, name, "CreatedBy", description, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Templates; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Templates" (id, name, content, channel, "CreatedBy", description, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: TransactionReplications; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."TransactionReplications" (id, "AgencyId", "ExternalMerchantCustomerId", "orderId", "txnId", "txnUuid", amount, "customerEmail", "customerPhone", gateway, "cardIsin", "cardLastFourDigits", "nameOnCard", "paymentMethodType", "paymentMethod", udf1, udf2, udf3, udf4, udf5, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Transactions; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Transactions" (id, "AgencyId", "OrderId", "transactionRef", "PayeeVpaId", "payerVpa", "payeeVpa", "payerInfo", "payeeInfo", info, "selfInitiated", mode, amount, "upiRequestId", "BharatQRTxnId", type, status, "upiMsgId", "npciResponse", remarks, expiry, "transactionAt", currency, "custRef", "refUrl", "transactionSource", "qrSource", "settlementStatus", "CustomerId", "EmployeeId", "bankName", "accountNumber", "bankIFSC", udf1, udf2, udf3, udf4, udf5, "callbackUrl", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: TxnFilters; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."TxnFilters" (id, "OwnerId", active, context, "sourceType", type, value, metadata, "filterReason", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: Vpas; Type: TABLE DATA; Schema: jp_hpcl; Owner: cloud
--

COPY jp_hpcl."Vpas" (id, "PSPId", "PspAccountId", vpa, "normalizedVpa", "AgencyId", "CustomerId", status, "isPrimary", info, "sendNotifications", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: LoyaltyCustomers; Type: TABLE DATA; Schema: loyalty_schema; Owner: cloud
--

COPY loyalty_schema."LoyaltyCustomers" (id, "verifoneId", "deviceInfo", status, "mobileNumber", "gcmId", language, channel, version, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: LoyaltyStatics; Type: TABLE DATA; Schema: loyalty_schema; Owner: cloud
--

COPY loyalty_schema."LoyaltyStatics" (id, key, type, source, value, "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: LoyaltyTokens; Type: TABLE DATA; Schema: loyalty_schema; Owner: cloud
--

COPY loyalty_schema."LoyaltyTokens" (id, token, verified, "expiresIn", "EntityId", "entityType", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: LoyaltyTransactions; Type: TABLE DATA; Schema: loyalty_schema; Owner: cloud
--

COPY loyalty_schema."LoyaltyTransactions" (id, "AgencyId", "payerVpa", amount, "payeeVpa", "payeeName", source, status, info, mode, cashback, reward, "rewardStatus", "custRef", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Data for Name: LoyaltyVPAs; Type: TABLE DATA; Schema: loyalty_schema; Owner: cloud
--

COPY loyalty_schema."LoyaltyVPAs" (id, "CustomerId", vpa, "isVerified", "isPrimary", "createdAt", "updatedAt") FROM stdin;
\.


--
-- Name: ActivationCodes ActivationCodes_code_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."ActivationCodes"
    ADD CONSTRAINT "ActivationCodes_code_key" UNIQUE (code);


--
-- Name: ActivationCodes ActivationCodes_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."ActivationCodes"
    ADD CONSTRAINT "ActivationCodes_pkey" PRIMARY KEY (id);


--
-- Name: ActivityLogs ActivityLogs_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."ActivityLogs"
    ADD CONSTRAINT "ActivityLogs_pkey" PRIMARY KEY (id);


--
-- Name: Addresses Addresses_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Addresses"
    ADD CONSTRAINT "Addresses_pkey" PRIMARY KEY (id);


--
-- Name: Agencies Agencies_MerchantAccountId_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Agencies"
    ADD CONSTRAINT "Agencies_MerchantAccountId_key" UNIQUE ("MerchantAccountId");


--
-- Name: Agencies Agencies_MerchantId_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Agencies"
    ADD CONSTRAINT "Agencies_MerchantId_key" UNIQUE ("MerchantId");


--
-- Name: Agencies Agencies_code_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Agencies"
    ADD CONSTRAINT "Agencies_code_key" UNIQUE (code);


--
-- Name: Agencies Agencies_merchantCode_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Agencies"
    ADD CONSTRAINT "Agencies_merchantCode_key" UNIQUE ("merchantCode");


--
-- Name: Agencies Agencies_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Agencies"
    ADD CONSTRAINT "Agencies_pkey" PRIMARY KEY (id);


--
-- Name: Applications Applications_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Applications"
    ADD CONSTRAINT "Applications_pkey" PRIMARY KEY (id);


--
-- Name: BankAccounts BankAccounts_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."BankAccounts"
    ADD CONSTRAINT "BankAccounts_pkey" PRIMARY KEY (id);


--
-- Name: Blacklists Blacklists_OwnerId_context_value_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Blacklists"
    ADD CONSTRAINT "Blacklists_OwnerId_context_value_key" UNIQUE ("OwnerId", context, value);


--
-- Name: Blacklists Blacklists_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Blacklists"
    ADD CONSTRAINT "Blacklists_pkey" PRIMARY KEY (id);


--
-- Name: Configurations Configurations_key_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Configurations"
    ADD CONSTRAINT "Configurations_key_key" UNIQUE (key);


--
-- Name: Configurations Configurations_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Configurations"
    ADD CONSTRAINT "Configurations_pkey" PRIMARY KEY (id);


--
-- Name: CustomData CustomData_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."CustomData"
    ADD CONSTRAINT "CustomData_pkey" PRIMARY KEY (id);


--
-- Name: Customers Customers_normalizedVpa_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Customers"
    ADD CONSTRAINT "Customers_normalizedVpa_key" UNIQUE ("normalizedVpa");


--
-- Name: Customers Customers_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Customers"
    ADD CONSTRAINT "Customers_pkey" PRIMARY KEY (id);


--
-- Name: Customers Customers_vpa_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Customers"
    ADD CONSTRAINT "Customers_vpa_key" UNIQUE (vpa);


--
-- Name: Devices Devices_deviceId_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Devices"
    ADD CONSTRAINT "Devices_deviceId_key" UNIQUE ("deviceId");


--
-- Name: Devices Devices_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Devices"
    ADD CONSTRAINT "Devices_pkey" PRIMARY KEY (id);


--
-- Name: Emails Emails_email_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Emails"
    ADD CONSTRAINT "Emails_email_key" UNIQUE (email);


--
-- Name: Emails Emails_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Emails"
    ADD CONSTRAINT "Emails_pkey" PRIMARY KEY (id);


--
-- Name: Employees Employees_code_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Employees"
    ADD CONSTRAINT "Employees_code_key" UNIQUE (code);


--
-- Name: Employees Employees_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Employees"
    ADD CONSTRAINT "Employees_pkey" PRIMARY KEY (id);


--
-- Name: EntityTags EntityTags_EntityId_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."EntityTags"
    ADD CONSTRAINT "EntityTags_EntityId_key" UNIQUE ("EntityId");


--
-- Name: EntityTags EntityTags_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."EntityTags"
    ADD CONSTRAINT "EntityTags_pkey" PRIMARY KEY (id);


--
-- Name: Events Events_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Events"
    ADD CONSTRAINT "Events_pkey" PRIMARY KEY (id);


--
-- Name: ExternalMerchantCustomers ExternalMerchantCustomers_MerchantId_CustomerId_legacyCusto_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."ExternalMerchantCustomers"
    ADD CONSTRAINT "ExternalMerchantCustomers_MerchantId_CustomerId_legacyCusto_key" UNIQUE ("MerchantId", "CustomerId", "legacyCustomerId", "OrganizationId");


--
-- Name: ExternalMerchantCustomers ExternalMerchantCustomers_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."ExternalMerchantCustomers"
    ADD CONSTRAINT "ExternalMerchantCustomers_pkey" PRIMARY KEY (id);


--
-- Name: Logins Logins_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Logins"
    ADD CONSTRAINT "Logins_pkey" PRIMARY KEY (id);


--
-- Name: MerchantKeys MerchantKeys_accessId_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."MerchantKeys"
    ADD CONSTRAINT "MerchantKeys_accessId_key" UNIQUE ("accessId");


--
-- Name: MerchantKeys MerchantKeys_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."MerchantKeys"
    ADD CONSTRAINT "MerchantKeys_pkey" PRIMARY KEY (id);


--
-- Name: MobileNumbers MobileNumbers_mobileNumber_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."MobileNumbers"
    ADD CONSTRAINT "MobileNumbers_mobileNumber_key" UNIQUE ("mobileNumber");


--
-- Name: MobileNumbers MobileNumbers_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."MobileNumbers"
    ADD CONSTRAINT "MobileNumbers_pkey" PRIMARY KEY (id);


--
-- Name: OfferRedemptions OfferRedemptions_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."OfferRedemptions"
    ADD CONSTRAINT "OfferRedemptions_pkey" PRIMARY KEY (id);


--
-- Name: Offers Offers_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Offers"
    ADD CONSTRAINT "Offers_pkey" PRIMARY KEY (id);


--
-- Name: Orders Orders_AgencyId_merchantOrderId_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Orders"
    ADD CONSTRAINT "Orders_AgencyId_merchantOrderId_key" UNIQUE ("AgencyId", "merchantOrderId");


--
-- Name: Orders Orders_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Orders"
    ADD CONSTRAINT "Orders_pkey" PRIMARY KEY (id);


--
-- Name: Organizations Organizations_code_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Organizations"
    ADD CONSTRAINT "Organizations_code_key" UNIQUE (code);


--
-- Name: Organizations Organizations_handle_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Organizations"
    ADD CONSTRAINT "Organizations_handle_key" UNIQUE (handle);


--
-- Name: Organizations Organizations_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Organizations"
    ADD CONSTRAINT "Organizations_pkey" PRIMARY KEY (id);


--
-- Name: PSPAccounts PSPAccounts_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."PSPAccounts"
    ADD CONSTRAINT "PSPAccounts_pkey" PRIMARY KEY (id);


--
-- Name: PSPResponses PSPResponses_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."PSPResponses"
    ADD CONSTRAINT "PSPResponses_pkey" PRIMARY KEY (id);


--
-- Name: PSPs PSPs_handle_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."PSPs"
    ADD CONSTRAINT "PSPs_handle_key" UNIQUE (handle);


--
-- Name: PSPs PSPs_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."PSPs"
    ADD CONSTRAINT "PSPs_pkey" PRIMARY KEY (id);


--
-- Name: PSPs PSPs_shortCode_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."PSPs"
    ADD CONSTRAINT "PSPs_shortCode_key" UNIQUE ("shortCode");


--
-- Name: PSPs PSPs_vpaHandle_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."PSPs"
    ADD CONSTRAINT "PSPs_vpaHandle_key" UNIQUE ("vpaHandle");


--
-- Name: Pincodes Pincodes_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Pincodes"
    ADD CONSTRAINT "Pincodes_pkey" PRIMARY KEY (id);


--
-- Name: PingChecks PingChecks_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."PingChecks"
    ADD CONSTRAINT "PingChecks_pkey" PRIMARY KEY (id);


--
-- Name: ProcessTracker ProcessTracker_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."ProcessTracker"
    ADD CONSTRAINT "ProcessTracker_pkey" PRIMARY KEY (id);


--
-- Name: Processes Processes_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Processes"
    ADD CONSTRAINT "Processes_pkey" PRIMARY KEY (id);


--
-- Name: Refunds Refunds_AgencyId_refundRefId_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Refunds"
    ADD CONSTRAINT "Refunds_AgencyId_refundRefId_key" UNIQUE ("AgencyId", "refundRefId");


--
-- Name: Refunds Refunds_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Refunds"
    ADD CONSTRAINT "Refunds_pkey" PRIMARY KEY (id);


--
-- Name: RegistrationTokens RegistrationTokens_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."RegistrationTokens"
    ADD CONSTRAINT "RegistrationTokens_pkey" PRIMARY KEY (id);


--
-- Name: RegistrationTokens RegistrationTokens_token_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."RegistrationTokens"
    ADD CONSTRAINT "RegistrationTokens_token_key" UNIQUE (token);


--
-- Name: Rules Rules_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Rules"
    ADD CONSTRAINT "Rules_pkey" PRIMARY KEY (id);


--
-- Name: SavedPaymentMethods SavedPaymentMethods_MerchantId_OrganizationId_ExternalMerch_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."SavedPaymentMethods"
    ADD CONSTRAINT "SavedPaymentMethods_MerchantId_OrganizationId_ExternalMerch_key" UNIQUE ("MerchantId", "OrganizationId", "ExternalMerchantCustomerId", "paymentMethod", value);


--
-- Name: SavedPaymentMethods SavedPaymentMethods_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."SavedPaymentMethods"
    ADD CONSTRAINT "SavedPaymentMethods_pkey" PRIMARY KEY (id);


--
-- Name: SettlementAccounts SettlementAccounts_VpaId_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."SettlementAccounts"
    ADD CONSTRAINT "SettlementAccounts_VpaId_key" UNIQUE ("VpaId");


--
-- Name: SettlementAccounts SettlementAccounts_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."SettlementAccounts"
    ADD CONSTRAINT "SettlementAccounts_pkey" PRIMARY KEY (id);


--
-- Name: SettlementBatches SettlementBatches_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."SettlementBatches"
    ADD CONSTRAINT "SettlementBatches_pkey" PRIMARY KEY (id);


--
-- Name: SettlementBatches SettlementBatches_utrNo_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."SettlementBatches"
    ADD CONSTRAINT "SettlementBatches_utrNo_key" UNIQUE ("utrNo");


--
-- Name: SettlementLogs SettlementLogs_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."SettlementLogs"
    ADD CONSTRAINT "SettlementLogs_pkey" PRIMARY KEY (id);


--
-- Name: Settlements Settlements_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Settlements"
    ADD CONSTRAINT "Settlements_pkey" PRIMARY KEY (id);


--
-- Name: ShipmentDealers ShipmentDealers_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."ShipmentDealers"
    ADD CONSTRAINT "ShipmentDealers_pkey" PRIMARY KEY (id);


--
-- Name: ShipmentDealers ShipmentDealers_shortcode_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."ShipmentDealers"
    ADD CONSTRAINT "ShipmentDealers_shortcode_key" UNIQUE (shortcode);


--
-- Name: Shipments Shipments_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Shipments"
    ADD CONSTRAINT "Shipments_pkey" PRIMARY KEY (id);


--
-- Name: StarterKits StarterKits_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."StarterKits"
    ADD CONSTRAINT "StarterKits_pkey" PRIMARY KEY (id);


--
-- Name: States States_name_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."States"
    ADD CONSTRAINT "States_name_key" UNIQUE (name);


--
-- Name: States States_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."States"
    ADD CONSTRAINT "States_pkey" PRIMARY KEY (id);


--
-- Name: States States_shortCode_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."States"
    ADD CONSTRAINT "States_shortCode_key" UNIQUE ("shortCode");


--
-- Name: StaticAssets StaticAssets_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."StaticAssets"
    ADD CONSTRAINT "StaticAssets_pkey" PRIMARY KEY (id);


--
-- Name: Tags Tags_name_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Tags"
    ADD CONSTRAINT "Tags_name_key" UNIQUE (name);


--
-- Name: Tags Tags_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Tags"
    ADD CONSTRAINT "Tags_pkey" PRIMARY KEY (id);


--
-- Name: Templates Templates_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Templates"
    ADD CONSTRAINT "Templates_pkey" PRIMARY KEY (id);


--
-- Name: TransactionReplications TransactionReplications_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."TransactionReplications"
    ADD CONSTRAINT "TransactionReplications_pkey" PRIMARY KEY (id);


--
-- Name: Transactions Transactions_OrderId_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Transactions"
    ADD CONSTRAINT "Transactions_OrderId_key" UNIQUE ("OrderId");


--
-- Name: Transactions Transactions_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Transactions"
    ADD CONSTRAINT "Transactions_pkey" PRIMARY KEY (id);


--
-- Name: TxnFilters TxnFilters_OwnerId_context_value_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."TxnFilters"
    ADD CONSTRAINT "TxnFilters_OwnerId_context_value_key" UNIQUE ("OwnerId", context, value);


--
-- Name: TxnFilters TxnFilters_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."TxnFilters"
    ADD CONSTRAINT "TxnFilters_pkey" PRIMARY KEY (id);


--
-- Name: Vpas Vpas_normalizedVpa_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Vpas"
    ADD CONSTRAINT "Vpas_normalizedVpa_key" UNIQUE ("normalizedVpa");


--
-- Name: Vpas Vpas_pkey; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Vpas"
    ADD CONSTRAINT "Vpas_pkey" PRIMARY KEY (id);


--
-- Name: Vpas Vpas_vpa_key; Type: CONSTRAINT; Schema: jp_hpcl; Owner: cloud
--

ALTER TABLE ONLY jp_hpcl."Vpas"
    ADD CONSTRAINT "Vpas_vpa_key" UNIQUE (vpa);


--
-- Name: LoyaltyCustomers LoyaltyCustomers_gcmId_key; Type: CONSTRAINT; Schema: loyalty_schema; Owner: cloud
--

ALTER TABLE ONLY loyalty_schema."LoyaltyCustomers"
    ADD CONSTRAINT "LoyaltyCustomers_gcmId_key" UNIQUE ("gcmId");


--
-- Name: LoyaltyCustomers LoyaltyCustomers_mobileNumber_key; Type: CONSTRAINT; Schema: loyalty_schema; Owner: cloud
--

ALTER TABLE ONLY loyalty_schema."LoyaltyCustomers"
    ADD CONSTRAINT "LoyaltyCustomers_mobileNumber_key" UNIQUE ("mobileNumber");


--
-- Name: LoyaltyCustomers LoyaltyCustomers_pkey; Type: CONSTRAINT; Schema: loyalty_schema; Owner: cloud
--

ALTER TABLE ONLY loyalty_schema."LoyaltyCustomers"
    ADD CONSTRAINT "LoyaltyCustomers_pkey" PRIMARY KEY (id);


--
-- Name: LoyaltyCustomers LoyaltyCustomers_verifoneId_key; Type: CONSTRAINT; Schema: loyalty_schema; Owner: cloud
--

ALTER TABLE ONLY loyalty_schema."LoyaltyCustomers"
    ADD CONSTRAINT "LoyaltyCustomers_verifoneId_key" UNIQUE ("verifoneId");


--
-- Name: LoyaltyStatics LoyaltyStatics_key_type_source_key; Type: CONSTRAINT; Schema: loyalty_schema; Owner: cloud
--

ALTER TABLE ONLY loyalty_schema."LoyaltyStatics"
    ADD CONSTRAINT "LoyaltyStatics_key_type_source_key" UNIQUE (key, type, source);


--
-- Name: LoyaltyStatics LoyaltyStatics_pkey; Type: CONSTRAINT; Schema: loyalty_schema; Owner: cloud
--

ALTER TABLE ONLY loyalty_schema."LoyaltyStatics"
    ADD CONSTRAINT "LoyaltyStatics_pkey" PRIMARY KEY (id);


--
-- Name: LoyaltyTokens LoyaltyTokens_pkey; Type: CONSTRAINT; Schema: loyalty_schema; Owner: cloud
--

ALTER TABLE ONLY loyalty_schema."LoyaltyTokens"
    ADD CONSTRAINT "LoyaltyTokens_pkey" PRIMARY KEY (id);


--
-- Name: LoyaltyTokens LoyaltyTokens_token_key; Type: CONSTRAINT; Schema: loyalty_schema; Owner: cloud
--

ALTER TABLE ONLY loyalty_schema."LoyaltyTokens"
    ADD CONSTRAINT "LoyaltyTokens_token_key" UNIQUE (token);


--
-- Name: LoyaltyTransactions LoyaltyTransactions_pkey; Type: CONSTRAINT; Schema: loyalty_schema; Owner: cloud
--

ALTER TABLE ONLY loyalty_schema."LoyaltyTransactions"
    ADD CONSTRAINT "LoyaltyTransactions_pkey" PRIMARY KEY (id);


--
-- Name: LoyaltyVPAs LoyaltyVPAs_CustomerId_key; Type: CONSTRAINT; Schema: loyalty_schema; Owner: cloud
--

ALTER TABLE ONLY loyalty_schema."LoyaltyVPAs"
    ADD CONSTRAINT "LoyaltyVPAs_CustomerId_key" UNIQUE ("CustomerId");


--
-- Name: LoyaltyVPAs LoyaltyVPAs_pkey; Type: CONSTRAINT; Schema: loyalty_schema; Owner: cloud
--

ALTER TABLE ONLY loyalty_schema."LoyaltyVPAs"
    ADD CONSTRAINT "LoyaltyVPAs_pkey" PRIMARY KEY (id);


--
-- Name: LoyaltyVPAs LoyaltyVPAs_vpa_key; Type: CONSTRAINT; Schema: loyalty_schema; Owner: cloud
--

ALTER TABLE ONLY loyalty_schema."LoyaltyVPAs"
    ADD CONSTRAINT "LoyaltyVPAs_vpa_key" UNIQUE (vpa);


--
-- Name: TransactionReplications_idx_cardIsin; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_cardIsin" ON jp_hpcl."TransactionReplications" USING btree ("cardIsin");


--
-- Name: TransactionReplications_idx_cardLastFourDigits; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_cardLastFourDigits" ON jp_hpcl."TransactionReplications" USING btree ("cardLastFourDigits");


--
-- Name: TransactionReplications_idx_customerEmail; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_customerEmail" ON jp_hpcl."TransactionReplications" USING btree ("customerEmail");


--
-- Name: TransactionReplications_idx_customerPhone; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_customerPhone" ON jp_hpcl."TransactionReplications" USING btree ("customerPhone");


--
-- Name: TransactionReplications_idx_gateway; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_gateway" ON jp_hpcl."TransactionReplications" USING btree (gateway);


--
-- Name: TransactionReplications_idx_nameOnCard; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_nameOnCard" ON jp_hpcl."TransactionReplications" USING btree ("nameOnCard");


--
-- Name: TransactionReplications_idx_orderId; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_orderId" ON jp_hpcl."TransactionReplications" USING btree ("orderId");


--
-- Name: TransactionReplications_idx_paymentMethod; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_paymentMethod" ON jp_hpcl."TransactionReplications" USING btree ("paymentMethod");


--
-- Name: TransactionReplications_idx_paymentMethodType; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_paymentMethodType" ON jp_hpcl."TransactionReplications" USING btree ("paymentMethodType");


--
-- Name: TransactionReplications_idx_txnId; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_txnId" ON jp_hpcl."TransactionReplications" USING btree ("txnId");


--
-- Name: TransactionReplications_idx_txnUuid; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_txnUuid" ON jp_hpcl."TransactionReplications" USING btree ("txnUuid");


--
-- Name: TransactionReplications_idx_udf1; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_udf1" ON jp_hpcl."TransactionReplications" USING btree (udf1);


--
-- Name: TransactionReplications_idx_udf2; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_udf2" ON jp_hpcl."TransactionReplications" USING btree (udf2);


--
-- Name: TransactionReplications_idx_udf3; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_udf3" ON jp_hpcl."TransactionReplications" USING btree (udf3);


--
-- Name: TransactionReplications_idx_udf4; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_udf4" ON jp_hpcl."TransactionReplications" USING btree (udf4);


--
-- Name: TransactionReplications_idx_udf5; Type: INDEX; Schema: jp_hpcl; Owner: cloud
--

CREATE INDEX "TransactionReplications_idx_udf5" ON jp_hpcl."TransactionReplications" USING btree (udf5);


--
-- PostgreSQL database dump complete
--

