
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
  name: 'SolutionOutput',
  data() {
    return {
      //messages: [],
    };
  },
  props: {
    solverservice: Object
  },
  computed: {
    messages() {
      return this.solverservice?.messages || ['Awaiting puzzle'];
    }
  },
  mounted() {
    //alert('mounted:' + this.solverservice);
    // this.solveService.addListener({
    //   onError: () => {
    //     console.log('Error occurred');
    //   },
    //   onOpen: () => {
    //     console.log('Connection established');
    //   },
    //   onBegin: (data) => {
    //     this.messages = [];
    //     this.messages.push(data);
    //   },
    //   onEnd: (data) => {
    //     this.messages.push(data);
    //   },
    //   onMessage: (data) => {
    //     this.messages.push(data);
    //   },
    //   onSolution: (data) => {
    //     this.messages.push(data);
    //   },
    // });
  },
  methods: {
    // startSolve() {
    //   this.solveService.solve();
    // },
  },
};

</script>


<style scoped>

#output-panel {
  background-color: black;
  color: white;
  font-family: 'Cutive Mono', serif;
  font-size: smaller;
}

</style>
