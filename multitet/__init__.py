import os
from libavg.utils import getMediaDir, createImagePreviewNode
from multitet import Multitet

__all__ = [ 'apps', ]

def createPreviewNode(maxSize):
    filename = os.path.join(getMediaDir(__file__), 'preview.png')
    return createImagePreviewNode(maxSize, absHref = filename)

apps = (
        {'class': Multitet,
            'createPreviewNode': createPreviewNode},
        )
