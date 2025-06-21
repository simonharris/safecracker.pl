<script setup>
import SolverService from '@/services/SolverService.js';
</script>

<template>
  <div class="upload-component">
    <form @submit.prevent="handleSubmit">
      <input type="file" ref="fileInput" />
      <button>Upload</button>
    </form>
    <progress v-if="solverservice.uploading" :value="solverservice.progress" max="100">{{ solverservice.progress }}%</progress>
  </div>
</template>

<script>

export default {
  name: 'FileUploadForm',
  data() {
    return {
      uploading: false,
      progress: 0,
      solverservice: new SolverService(),
    }
  },
  // props: {
  //   solverservice: Object
  // },
  methods: {
    handleSubmit() {
      const file = this.$refs.fileInput.files[0];

      this.$emit('form-submitted', 'hi');
      if (!file) return;

      this.solverservice.upload(file).then((response) => {
          // console.log('From promise:', response);
          // console.log('Response keys:', Object.keys(response));
          // console.log('Response data:', response.data);
            this.solverservice.solveUpload(response.data);
      });
    }
  }
}
</script>