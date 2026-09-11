import { defineConfig } from 'vite'

export default defineConfig({
  root: new URL('.', import.meta.url).pathname,
  publicDir: 'public',
  build: {
    outDir: 'dist',
    emptyOutDir: true,
  },
  server: {
    port: 5557,
    host: '0.0.0.0',
    fs: {
      allow: ['..', '../../rbt-widgets/test-pdfs'],
    },
  },
})
