
<template>
<div class="upload-component">
  <form @submit.prevent="handleSubmit">
    <input type="file" />
    <button>Upload</button>
  </form>
</div>
</template>

<script>
export default {
  methods: {
    handleSubmit() {
      alert("You clicked me!");
    }
  }
}
</script>