import { defineConfig } from 'vite'
import tailwindcss from '@tailwindcss/vite'
import elmPlugin from 'vite-plugin-elm'
import { VitePWA } from 'vite-plugin-pwa'

export default defineConfig({
  plugins: [
    elmPlugin(),
    tailwindcss(),
    VitePWA({
      registerType: 'autoUpdate',
      injectRegister: 'auto',
      workbox: {
        globPatterns: ['**/*.{js,css,html,ico,png,svg,json,ttf,woff,woff2}'],
        navigateFallback: '/index.html',
        runtimeCaching: [
          {
            urlPattern: /^https:\/\/firestore\.googleapis\.com\/v1\/projects\/qelm-analytics\/databases\/\(default\)\/documents\/sessions/,
            method: 'POST',
            handler: 'NetworkOnly',
            options: {
              backgroundSync: {
                name: 'analytics-queue',
                options: {
                  maxRetentionTime: 24 * 60 // Retry for up to 24 hours
                }
              }
            }
          }
        ]
      },
      manifest: {
        name: 'Qelm',
        short_name: 'Qelm',
        description: 'A modern, minimalist Amharic touch-typing tutor.',
        theme_color: '#292524',
        background_color: '#ffffff',
        display: 'standalone',
        orientation: 'portrait',
        icons: [
          {
            src: '/icon.svg',
            sizes: '192x192 512x512',
            type: 'image/svg+xml',
            purpose: 'any maskable'
          }
        ]
      }
    })
  ],
})
