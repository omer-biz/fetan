<!DOCTYPE html>
<html>
<head>
    <title>Email Example</title>
</head>
<body>
    <h1>{{ $emailData['subject'] }}</h1>

    <p>From: {{ $emailData['name'] }}</p>
    <p>Email: {{ $emailData['email'] }}</p>

    <p>{{ $emailData['message'] }}</p>
</body>
</html>
