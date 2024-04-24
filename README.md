# Snake

A configurable N-body physics simulator. 

![Binary Star Simulation](https://raw.githubusercontent.com/Archaversine/snake/main/binary.png)

Screenshot of the simulation of `binary.json`.

## Usage

In your window explorer, click and drag the JSON file onto the snake executable. Otherwise, use the JSON file as an argument to the executable from the command line.

## Custom Simulations

Simulations are defined in JSON files. The following is an example of a minimal 
simulation with no bodies:

```json
{
    "entities": [],
    "windowWidth": 1000,
    "windowHeight": 800,
    "windowTitle": "Custom Simulation",
    "framerate": 60,
    "maxTrailLength": 100
}
```

In order for a simulation to be valid, all of the fields must be present.

### Entities 

In the `entities` field, you can define the bodies in the simulation. Each body is defined as follows:

```json
{
    "title": "Star",
    "position": {
        "x": 500,
        "y": 400
    },
    "velocity": {
        "x": 0,
        "y": 0 
    },
    "mass": 100000,
    "size": 20,
    "immovable": false,
    "color": {
        "r": 255,
        "g": 255,
        "b": 0,
        "a": 255
    }
}
```

Again, all of these fields are required for a body to be valid. The `entities`
field can contain any number of bodies.

You can also use the provided `sample.json` and `binary.json` as examples.

Once you have finished creating your simulation, save it as a JSON file 
and pass it as parameter to the executable.
