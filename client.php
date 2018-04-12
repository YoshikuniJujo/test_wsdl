<?php

$client = new SoapClient(
	"getPrice.wsdl",
#	"http://localhost/naiyo.wsdl",
	array('location' => "http://localhost/hoge.php") );

$functions = $client->__getFunctions();
var_dump($functions);

?>
