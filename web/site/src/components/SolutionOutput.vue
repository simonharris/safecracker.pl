
<template>
<hr>

<div id="output-panel" ref="outputPanel" class="p-2">
  <div v-for="(message, index) in messages" :key="index" v-bind:class="message.type">{{ message.content }}</div>
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
  height: 24em;
  overflow-y: scroll;
}

#output-panel::-webkit-scrollbar {
  width: 10px;
  height: 10px;
  background-color: #f0f0f0;
}

#output-panel::-webkit-scrollbar-thumb {
  background-color: #ccc;
  border-radius: 5px;
}

.msg-phase { color: #4AF626; }
.msg-progress { color: orange; }
.msg-solution {  font-family: 'Sixtyfour Convergence', sans-serif; }


</style>
