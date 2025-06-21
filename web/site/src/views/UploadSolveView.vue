<template>
  <h1>Solve from Upload</h1>

  <div>
    home page
    <!-- <FileUploadForm  /> -->

<!-- <p>
<button @click="solvePuzzle" id="start-button">Start</button>
<button @click="stopEventSource" id="stop-button">Stop</button>
</p> -->


    <!-- image-preview / -->
    <!-- <SolutionOutput :messages="messages" /> -->
  </div>


</template>

<script>
// // @ is an alias to /src
// import FileUploadForm from '@/components/FileUploadForm.vue'
// import SolutionOutput from '@/components/SolutionOutput.vue'

// export default {
//   name: 'UploadSolveView',
//   components: {
//     FileUploadForm,
//     SolutionOutput,
//   },
//   data() {
//     return {
//       eventSource: null,
//       messages: [],
//     }
//   },
//   setup() {
//     // ...
//   },
//   mounted() {
//     // ...
//   },
//   computed: {
//     isHomeRoute() {
//       return this.$route.name === 'home'
//     }
//   },
//   methods: {
//     // solvePuzzle() {

//     //   this.eventSource = new EventSource('http://127.0.0.1:5000/solve/abcdef', { withCredentials: false });

//     //   this.eventSource.onerror = () => {
//     //     console.log('Error occurred. Ready state:', this.eventSource.readyState);
//     //   };
//     //   this.eventSource.onopen = () => {
//     //     console.log('Connection established');
//     //   };
//     //   this.eventSource.addEventListener('begin', (event) => {
//     //     console.log('Received "begin" event:', event.data);
//     //     this.messages = [];
//     //     this.messages.push(event.data);
//     //   });
//     //   this.eventSource.addEventListener('end', (event) => {
//     //     console.log('Received "end" event:', event.data);
//     //     this.eventSource.close();
//     //     this.messages.push(event.data);
//     //   });
//     //   this.eventSource.addEventListener('message', (event) => {
//     //     console.log('Received "message" event:', event.data);
//     //     this.messages.push(event.data);
//     //   });
//     //   this.eventSource.addEventListener('solution', (event) => {
//     //     console.log('Received "solution" event:', event.data);
//     //     this.messages.push(event.data);
//     //   });
//     // },
//     // stopEventSource() {
//     //   if (this.eventSource) {
//     //     this.eventSource.close();
//     //     this.eventSource = null; // Optional: reset the property
//     //   }
//     // }
//   }
// }
</script>


<style scoped>

</style>

