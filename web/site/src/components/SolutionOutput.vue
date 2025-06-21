
<template>
<hr>


<!-- button @click="solvePuzzle" id="start-button">Start</button>
<button @click="stopEventSource" id="stop-button">Stop</button -->


<div id="output-panel" ref="outputPanel"  class="output-panel h-50 overflow-auto p-2">
  <div v-for="(message, index) in messages" :key="index">{{ message }}</div>
</div>

</template>

<script>

export default {
  props: {
    messages: {
      type: Array,
      required: true
    }
  },
  // ...
}

</script>


<style scoped>

#output-panel {
  background-color: black;
  color: white;
  /* font-family: 'Sixtyfour', sans-serif; */
  font-size: smaller;
}

</style>
