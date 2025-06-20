<template>

<FileUpload />
<SolutionOutput />


<button @click="solvePuzzle" id="start-button">Start</button>
<button @click="stopEventSource" id="stop-button">Stop</button>


<div id="output-panel" ref="outputPanel"  class="output-panel h-50 overflow-auto p-2">
  <div v-for="(message, index) in messages" :key="index">{{ message }}</div>
</div>


</template>

<script>
// @ is an alias to /src
import FileUpload from '@/components/FileUpload.vue'
import SolutionOutput from '@/components/SolutionOutput.vue'

export default {
  name: 'HomeView',
  components: {
    FileUpload,
    SolutionOutput,
  },
  data() {
    return {
      eventSource: null,
      messages: [],
    }
  },
  setup() {
    // ...
  },
  mounted() {
    // ...
  },
  methods: {
    solvePuzzle() {

      this.eventSource = new EventSource('http://127.0.0.1:5000/solve/abcdef', { withCredentials: false });


      // this.eventSource.onmessage = (event) => {
      //     // if (event.data === 'begin') {
      //     //   console.log("Begin");
      //     // }

      //     // if (event.data === 'finished') {
      //     //   console.log("Closing");
      //     //   this.eventSource.close();
      //     // } else {
      //     //   console.log('Received event:', event.data);
      //     //   this.messages.push(event.data);
      //     //}
      // };
      this.eventSource.onerror = () => {
        console.log('Error occurred. Ready state:', this.eventSource.readyState);
      };
      this.eventSource.onopen = () => {
        console.log('Connection established');
      };
      this.eventSource.addEventListener('begin', (event) => {
        console.log('Received "begin" event:', event.data);
        this.messages = [];
        this.messages.push(event.data);
      });
      this.eventSource.addEventListener('end', (event) => {
        console.log('Received "end" event:', event.data);
        this.eventSource.close();
        this.messages.push(event.data);
      });
      this.eventSource.addEventListener('message', (event) => {
        console.log('Received "message" event:', event.data);
        this.messages.push(event.data);
      });
    },
    stopEventSource() {
      if (this.eventSource) {
        this.eventSource.close();
        this.eventSource = null; // Optional: reset the property
      }
    }
  }
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

