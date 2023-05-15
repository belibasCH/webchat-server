# fprod-webchat/server

This is the backend for a webchat application developed for the `fprod` module.
The webchat offers full end-to-end encrypted communication between users.

[toc]

## Usage

The server runs as a dockerized web application. Data is exchanged via [WebSockets](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API) and stored in a local [MongoDB](https://www.mongodb.com/) instance.
To run it, execute the following command. The server's API will then be available at `ws://localhost:8080`.

```bash
docker compose up
```

If you want to inspect the stored data, there's also an optional [mongo-express](https://github.com/mongo-express/mongo-express) service which exposes a UI for the database at `http://localhost:8081`:

```bash
docker compose up mongo-express
```

## Development

If you need to restart the API a lot (due to code changes), it may be advisable to run the database and its UI as background processes:

```bash
docker compose up -d db mongo-express
```

The server can then be run without requiring a restart of the database.

```bash
docker compose up --no-deps server
```

## API Messages

The WebSocket API expects requests to be in the following JSON-based format. Data sent by the API is using the same basic format.

```json
{ "type": "<message-type>", ... }
```

**Terminology**

Sending a message to the server will almost always trigger some sort of result, either to the client sending it, or to other clients.

* *Effects* are server messages that are sent to one or more specific clients because of an incoming client message. Unless otherwise specified, an effect is only sent to the client which sent the message that caused the effect itself.
* *Broadcasts* are server messages that are sent to all authenticated connections.
* *Errors* are server messages that indicate that a client message did not compute normally. Errors are always sent to the client which sent the corresponding, failed message.
* *Sending to a user* means that a message will be sent to all clients authenticated by the corresponding user.

### Client Message

The following messages may be sent to the server by any client.

#### `create_user`

Creates a new user account.

```json
{
    "type": "create_user",
    
    // The new user's name.
    "username": String,
    
    // The new user's password.
    "password": String
}
```

**Broadcasts**

* [`user_created`](#user_created). Note that unlike other broadcasts, this is also sent to the originating connection, no matter its authentication state.

**Errors**

* [`username_taken`](#username_taken), if there's already a user with the specified name (case-insensitive!).
* [`blank_password`](#blank_password), if the specified password is blank.

#### `change_username`

Changes the name of the authenticated user.

```json
{
    "type": "change_username",
    
    // The new username.
    "username": String
}
```

**Broadcasts**

* [`username_changed`](#username_changed)

**Errors**

* [`username_taken`](#username_taken), if there's already a user with the specified name (case-insensitive!).
* [`bad_request`](#bad_request), if the connection is not authenticated.

#### `change_password`

Changes the password of the authenticated user.

```json
{
    "type": "change_password",
    
    // The new password.
    "password": String
}
```

**Effects**

* [`password_changed`](#password_changed)

**Errors**

* [`blank_password`](#blank_password), if the specified password is blank.
* [`bad_request`](#bad_request), if the connection is not authenticated.

#### `delete_user`

Deletes the authenticated user and closes the connection.

```json
{
    "type": "delete_user"
}
```

**Broadcasts**

* [`user_deleted`](#user_deleted)

**Errors**

* [`bad_request`](#bad_request), if the connection is not authenticated.

#### `login`

Authenticates the current connection.

```json
{
    "type": "login",
    
    // The name of the user that's attempting to log in.
    "username": String,
    
    // The user's password.
    "password": String
}
```

**Effects**

* [`login_succeeded`](#login_succeeded)

**Broadcasts**

*  [`user_logged_in`](#user_logged_in), to all other clients (not to the newly authenticated client itself).

**Errors**

* [`login_failed`](#login_failed), if the login information did not match.
* [`bad_request`](#bad_request), if the connection is already authenticated.

#### `send`

Sends a chat message to another user.

```json
{
    "type": "send",
    
    // The id of the user to which the message will be sent.
    "receiver_id": UUID,
    
    // The encrypted text message.
    "text": String
}
```

**Effects**

* [`receive_message`](#receive_message), sent to the receiving user.
  The receiver's clients will receive this message each time when logging in, until its marked as [`received`](#received).

**Errors**

* [`not_found`](#not_found), if the receiving user does not exist.
* [`bad_request`](#bad_request), if the connection is not authenticated.

#### `received`

Marks a message as received. Received messages will not be sent to the receiving user's clients, but can still be loaded via [`load_chat`](#load_chat).

```json
{
    "type": "receive",
    
    // The id of the received message.
    "message_id": UUID,
}
```

**Effects**

* [`message_received`](#message_received), sent to the user which sent the message.

**Errors**

* [`not_found`](#message_not_found), if the specified message does not exist, or does not belong the the authenticated user.
* [`bad_request`](#bad_request), if the connection is not authenticated.

#### `read`

Marks a message as read.

```json
{
    "type": "read",
    
    // The id of the read message.
    "message_id": UUID,
}
```

**Effects**

* [`message_read`](#message_read), sent to the user which sent the message.

**Errors**

* [`not_found`](#message_not_found), if the specified message does not exist, or does not belong the the authenticated user.
* [`bad_request`](#bad_request), if the connection is not authenticated.

#### load_chats`load_users`

Requests all users to be loaded and sent to this client.

```json
{
    "type": "load_users"
}
```

**Effects**

* [`users_loaded`](#users_loaded)

**Errors**

* [`bad_request`](#bad_request), if the connection is not authenticated.

#### `load_chats`

Requests a list of all chats of the authenticated user to be loaded and sent to this client.

```json
{
    "type": "load_chats"
}
```

**Effects**

* [`chats_loaded`](#users_loaded)

**Errors**

* [`bad_request`](#bad_request), if the connection is not authenticated.

#### `load_chat`

Requests a chat of the authenticated user to be loaded and sent to this client.

```json
{
    "type": "load_chat",
    
    // The id of the other user with which the chat is shared.
    "user_id": UUID
}
```

**Effects**

* [`chat_loaded`](#chat_loaded)

**Errors**

* [`not_found`](#not_found), if the other chat user does not exist.

* [`bad_request`](#bad_request), if the connection is not authenticated.

### Server Messages

The following messages may be sent from the server to a specific client. 

#### `login_succeeded`

Sent to the client upon a successful [`login`](#login).
After receiving this, the connection can be regarded as authenticated.

```json
{
    "type": "login_succeeded",
    
    // The authenticated user.
    "user": User
}
```

#### `user_logged_in`

Broadcast to every authorized client when a user completes a [`login`](#login) without having any other active connections.
This can be used to mark a user as "online".

```json
{
    "type": "user_logged_in",
    
    // The id of the newly logged in user.
    "user_id": UUID
}
```

#### `user_logged_out`

Broadcast to every authorized client when the last authorized connection of a user is closed.
This can be used to mark a user as "offline".

```json
{
    "type": "user_logged_out",
    
    // The id of the newly logged out user.
    "user_id": UUID
}
```

#### `user_created`

Broadcast to every authorized client when a new user is created.

```json
{
    "type": "user_created",
    
    // The newly created user.
    "user": User
}
```

#### `username_changed`

Broadcast to every authorized client when a user changes its name.

```json
{
    "type": "username_changed",
    
    // The user that changed its name.
    "user": User
}
```

#### `password_changed`

Sent back to a client when it successfully changed its user's password.

```json
{
    "type": "password_changed"
}
```

#### `user_deleted`

Broadcast to every authorized client when a user gets deleted.

```json
{
    "type": "user_deleted",
       
    // The id of the now deleted user.
    "user_id": UUID
}
```

#### `message_sent`

Sent to a user when one of its clients [sent a message](#send) to another user.  

```json
{
    "type": "message_sent",
       
    // The message.
    "message": Message
}
```

#### `receive_message`

Sent to a user when a message is [sent_to_it](#send).
This message will be repeated every time the user logs in, until it gets marked as [`received`](#received).

```json
{
    "type": "receive_message",
       
    // The message.
    "message": Message
}
```

#### `received_message`

Sent to a user when a message that it has sent has been marked as  [`received`](#received).

```json
{
    "type": "message_received",
       
    // The message.
    "message": Message
}
```

#### `users_loaded`

Sent to a client after it has sent a [`load_users`](#load_users) request.

```json
{
    "type": "users_loaded",
       
    "users": [{
        // The user.
    	"user": User,
        
        // Whether the user is currently online.
        "is_online": Boolean
    }]
}
```

#### `chats_loaded`

Sent to a client after it has sent a [`load_chats`](#load_chats) request.

```json
{
    "type": "chats_loaded",
       
    "chats": [{
        // The other user with which this chat is shared.
    	"user": User,
        
        // The latest message exchanged in this chat.
        // May be either from either of the two users participating in this chat.
        "latest_message": Message,
        
        // The total amount of messages in this chat.
        "total_message_count": Int,
        
        // The amount of messages in this chat that are not marked as read.
        "unread_message_count": Int
    }]
}
```

#### `chat_loaded`

Sent to a client after it has sent a [`load_chat`](#load_chat) request.

```json
{
    "type": "chat_loaded",
  
    // All messages sent in this chat.
    "messages": [Message]
}
```



### Error Messages

An error message is a special [server message](#Server Messages) that indicates that client message could not be processed normally.
Error messages always take the following form.

```json
{ "type": "error", "error": "<error-type>", ... }
```

#### `bad_request`

The request is invalid, for one reason or another. This is very likely caused by a client-side logic error, and should ideally never happen in production code.

```json
{
	"type": "error",
    "error": "bad_request",
    
    // Description of what went wrong.
	"message": String
}
```

#### `login_failed`

A [`login`](#login) request failed, either due to a wrong username or password.

```json
{
	"type": "error",
    "error": "login_failed",
    
    // The username with which the login was attempted.
	"username": String
}
```

#### `username_taken`

Attempted to create a new user, or to rename a user, using a name that is already in use by another account.

```json
{
	"type": "error",
    "error": "username_taken",
    
    // The duplicate username.
	"username": String
}
```

#### `blank_password`

Attempted to create a new user with, or to change an existing password to, a blank one.

```json
{
	"type": "error",
    "error": "blank_password"
}
```

#### `not_found`

An id used in a client message did not point to any known record.

```json
{
	"type": "error",
    "error": "not_found",
    
    // The id that could not be resolved.
    "id": UUID,
    
    // The data type that has been searched.
	// Is either "User" or "Message".
    "data_type": String,
}
```

## API Types

### `User`

A user account, able to send and receive messages.

```json
{
    // The user's id.
    "id": UUID,
    
    // The user's unique name.
    "name": String
}
```

### `Message`

A chat message, sent from one user to another.

```js
{
    // The message's id.
    "id": UUID,

    // The id of the user that sent the message.
    "sender_id": UUID,

    // The id of the user that the message is sent to.
    "receiver_id": UUID,

    // The encrypted message content.
    "text": UUID,

    // The moment at which the message has been sent.
    "sent_at": DateTime,
    
    // The moment at which the message has first been received by the receiver.
    "received_at": DateTime | null,
    
    // The moment at which the receiving user first viewed the message.
    "read_at": DateTime | null
}
```

### `DateTime`

A moment in time. Always uses Coordinated Universal Time (UTC).

```json
"2023-05-06T15:02:36.815094164Z"
```

`UUID`

A string representing a universally unique identifier.

```json
"314258b5-a16f-46fe-93e5-82ca0e26e302"
```

