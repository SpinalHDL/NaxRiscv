#include <SDL2/SDL.h>
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>


class Framebuffer : public SimElement{
public:
    int width, height;
    uint32_t *pixels;
    SDL_Window* window;
    SDL_Renderer* renderer;
    SDL_Texture * texture;
    uint32_t x,y;
    uint32_t refreshCounter = 0;

    Framebuffer(int width, int height){
        this->width = width;
        this->height = height;
        x = y = 0;
        init();
    }

    virtual ~Framebuffer(){
        delete[] pixels;
        SDL_DestroyTexture(texture);
        SDL_DestroyRenderer(renderer);
        SDL_DestroyWindow(window);
        SDL_Quit();
    }

    void init(){

        /* Initialize SDL. */
        if (SDL_Init(SDL_INIT_VIDEO) < 0)
                return;

        /* Create the window where we will draw. */
        window = SDL_CreateWindow("Framebuffer",
                        SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                        width, height,
                        SDL_WINDOW_SHOWN);

        /* We must call SDL_CreateRenderer in order for draw calls to affect this window. */
        renderer = SDL_CreateRenderer(window, -1, 0);

        texture = SDL_CreateTexture(renderer,
            SDL_PIXELFORMAT_ABGR8888, SDL_TEXTUREACCESS_STATIC, width, height);
        pixels = new Uint32[width * height];
        memset(pixels, 0, width * height * sizeof(Uint32));
    }

    void refresh(){
        //cout << "Display refresh " << refreshCounter++ << endl;
        SDL_UpdateTexture(texture, NULL, pixels, width * sizeof(Uint32));
        SDL_RenderClear(renderer);
        SDL_RenderCopy(renderer, texture, NULL, NULL);
        SDL_RenderPresent(renderer);
        //memset(pixels, 0, width * height * sizeof(Uint32));
    }


    virtual void postCycle(){
        refreshCounter += 1;
        if(refreshCounter == 20000){
            refresh();
            refreshCounter = 0;
            SDL_Event e;
            while(SDL_PollEvent(&e) != 0){
                    if(e.type == SDL_QUIT) {
                    SDL_DestroyWindow(window);
                    SDL_Quit();
                    success();
                }
            }
        }
    }

    virtual void preCycle(){

    }
};
