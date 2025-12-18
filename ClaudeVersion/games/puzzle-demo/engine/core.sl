# Engine Core - Basic utilities for the puzzle game
# This file is read-only - learn from it!

# Vector utilities
fn vec2 x y:
    {x: x, y: y}

fn vec3 x y z:
    {x: x, y: y, z: z}

fn vec_add a b:
    if a.z:
        vec3(a.x + b.x, a.y + b.y, a.z + b.z)
    else:
        vec2(a.x + b.x, a.y + b.y)

fn vec_sub a b:
    if a.z:
        vec3(a.x - b.x, a.y - b.y, a.z - b.z)
    else:
        vec2(a.x - b.x, a.y - b.y)

fn vec_scale v s:
    if v.z:
        vec3(v.x * s, v.y * s, v.z * s)
    else:
        vec2(v.x * s, v.y * s)

# Entity state management
var entities = []

fn create_entity name props:
    let entity = {
        name: name,
        active: true,
        props: props
    }
    entities = push(entities, entity)
    entity

fn find_entity name:
    var result = 0
    for e in entities:
        if e.name == name:
            result = e
    result

# Game state
var game_state = {
    running: true,
    level: 1,
    score: 0,
    player: {
        pos: vec3(0, 0, 0),
        health: 100,
        inventory: []
    }
}

fn get_player:
    game_state.player

fn player_has item:
    contains(game_state.player.inventory, item)

fn give_player item:
    game_state.player.inventory = push(game_state.player.inventory, item)
    emit @player.got_item {item: item}

# Interaction system
fn can_interact entity:
    let player = get_player()
    let dist = distance(player.pos, entity.props.position)
    dist < 2

# Messaging
fn say message:
    emit @game.message {text: message}

fn notify message:
    emit @game.notification {text: message}
