
<template>
<hr>

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
    // ...
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
