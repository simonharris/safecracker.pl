
<template>
<hr>

<div id="output-panel" ref="outputPanel" class="p-2">
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
      return this.solverservice?.messages?.length > 0 ? this.solverservice.messages : ['Awaiting puzzle...'];
    }
  },
  mounted() {
    // ...
  },
  methods: {
    scrollToBottom() {
      this.$nextTick(() => {
        const outputPanel = this.$refs.outputPanel;
        outputPanel.scrollTop = outputPanel.scrollHeight;
      });
    },
  },
  watch: {
    messages: {
      deep: true,
      handler() {
        console.log('Scrollin');
        this.scrollToBottom();
      }
    }
  }
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
