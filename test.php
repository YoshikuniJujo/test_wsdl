<?php

$client = new SoapClient(
	"test.wsdl",
	array('cache_wsdl' => WSDL_CACHE_NONE) );

$functions = $client->__getFunctions();
var_dump($functions);

?>
