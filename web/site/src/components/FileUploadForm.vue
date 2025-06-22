<script setup>

</script>

<template>
  <!-- div class="form-group d-flex flex-wrap justify-content-center" -->
    <form @submit.prevent="handleSubmit" class="mb-3">
      <input type="file" ref="fileInput" class="form-control w-100 mb-2" />
      <button class="btn btn-secondary w-100">Solve</button>
    </form>
    <progress class="w-100" v-if="solverservice.uploading" :value="solverservice.progress" max="100">{{ solverservice.progress }}%</progress>
  <!-- /div -->
</template>

<script>

export default {
  name: 'FileUploadForm',
  data() {
    return {
      uploading: false,
      progress: 0,
    }
  },
  props: {
    solverservice: Object
  },
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