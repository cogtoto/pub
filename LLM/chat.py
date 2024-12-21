
import torch
from diffusers import FluxPipeline

pipe = FluxPipeline.from_pretrained("black-forest-labs/FLUX.1-dev", torch_dtype=torch.bfloat16,  token="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")
pipe.enable_model_cpu_offload() #save some VRAM by offloading the model to CPU. Remove this if you have enough GPU power
#pipe = pipe.to("cuda")

prompt = "a beagle with a blue collar sitting in a beach"
image = pipe(
    prompt,
    height=1024,
    width=1024,
    guidance_scale=3.5,
    num_inference_steps=50,
    max_sequence_length=512,
 #   generator=torch.Generator("cpu").manual_seed(0)
).images[0]
image.save("flux-dev.png")
