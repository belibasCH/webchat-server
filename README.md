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

### Client Message

The following messages may be sent to the server by any client.

**Terminology**

Sending a message to the server will almost always trigger some sort of result, either to the client sending it, or to other clients.

* *Effects* are server messages that are sent to one or more specific clients because of an incoming client message. Unless otherwise specified, an effect is only sent to the client which sent the message that caused the effect itself.
* *Broadcasts* are server messages that are sent to all authenticated connections.
* *Errors* are server messages that indicate that a client message did not compute normally. Errors are always sent to the client which sent the corresponding, failed message.

#### `create_user`

Creates a new user account.

```json
{
    "type": "create_user",
    
    // The new user's name.
    "username": "String",
    
    // The new user's password.
    "password": "String"
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
    "username": "String"
}
```

**Broadcasts**

* [`username_changed`](#username_changed)

**Errors**

* [`username_taken`](#username_taken), if there's already a user with the specified name (case-insensitive!).

#### `login`

Authenticates the current connection.

```json
{
    "type": "login",
    
    // The name of the user that's attempting to log in.
    "username": "String",
    
    // The user's password.
    "password": "String"
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
    "receiver_id": "UUID",
    
    // The encrypted text message.
    "text": "String"
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
    "message_id": "UUID",
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
    "message_id": "UUID",
}
```

**Effects**

* [`message_read`](#message_read), sent to the user which sent the message.

**Errors**

* [`not_found`](#message_not_found), if the specified message does not exist, or does not belong the the authenticated user.
* [`bad_request`](#bad_request), if the connection is not authenticated.

#### `load_users`

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
    "user_id": "UUID"
}
```

**Effects**

* [`chat_loaded`](#chat_loaded)

**Errors**

* [`not_found`](#not_found), if the other chat user does not exist.

* [`bad_request`](#bad_request), if the connection is not authenticated.

### Server Messages

The following messages may be sent from the server to a specific client. 

### Error Messages

