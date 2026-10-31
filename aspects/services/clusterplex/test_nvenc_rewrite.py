import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import nvenc_rewrite as nv  # noqa: E402

VAAPI_HLS = [
    "-codec:0", "h264",
    "-hwaccel:0", "vaapi",
    "-hwaccel_fallback_threshold:0", "10",
    "-hwaccel_device:0", "vaapi",
    "-hwaccel_output_format:0", "vaapi",
    "-init_hw_device", "vaapi=vaapi:/dev/dri/renderD128",
    "-filter_hw_device", "vaapi",
    "-i", "/mnt/hdd/Media/Movies/x.mkv",
    "-filter_complex",
    "[0:0]scale_vaapi=w=1280:h=720:format=nv12:mode=hq[0];[0:1]aresample=async=1:ochl='stereo':rematrix_maxval=0.000000dB:osr=48000[1]",
    "-map", "[0]",
    "-codec:0", "h264_vaapi",
    "-rc_mode:0", "VBR",
    "-low_power:0", "1",
    "-b:0", "4000k",
    "-map", "[1]",
    "-codec:1", "aac",
    "-progressurl", "http://192.168.0.26:32499/video/:/transcode/session/abc/progress",
    "dash",
]


def rw(args: list[str]) -> list[str]:
    return list(nv.rewrite([nv.Arg(a) for a in args]))


def after(args, flag):
    return args[args.index(flag) + 1]


class Rewrite(unittest.TestCase):
    def test_full_session(self):
        out = rw(VAAPI_HLS)
        self.assertEqual(after(out, "-hwaccel:0"), "cuda")
        self.assertEqual(after(out, "-hwaccel_output_format:0"), "cuda")
        self.assertEqual(after(out, "-init_hw_device"), "cuda=cuda:0")
        self.assertEqual(after(out, "-hwaccel_device:0"), "cuda")
        self.assertEqual(after(out, "-filter_hw_device"), "cuda")
        self.assertIn("[0:0]scale_cuda=w=1280:h=720:format=nv12[0]",
                      after(out, "-filter_complex"))
        self.assertIn("aresample=async=1:ochl='stereo'",
                      after(out, "-filter_complex"))
        self.assertEqual(after(out, "-rc:0"), "vbr")
        self.assertNotIn("-low_power:0", out)
        self.assertIn("h264_nvenc", out)
        self.assertFalse(any("vaapi" in a or "/dev/dri" in a for a in out))

    def test_untouched_software_session(self):
        args = ["-i", "in.mkv", "-codec:0", "libx264", "-vf", "scale=w=1:h=2", "out"]
        self.assertEqual(rw(args), args)

    def test_derived_drm_device(self):
        out = rw(["-init_hw_device", "drm=dr:/dev/dri/renderD128",
                  "-init_hw_device", "vaapi=va@dr",
                  "-filter_hw_device", "va"])
        self.assertEqual(out, ["-init_hw_device", "cuda=va:0",
                               "-filter_hw_device", "va"])

    def test_deinterlace(self):
        g = nv.rewrite_graph(nv.FilterGraph("[0:0]deinterlace_vaapi=rate=field[a]"))
        self.assertEqual(g, "[0:0]yadif_cuda=mode=send_field[a]")

    def test_format_list(self):
        g = nv.rewrite_graph(nv.FilterGraph("format=nv12|vaapi,hwupload"))
        self.assertEqual(g, "format=nv12|cuda,hwupload")

    def test_positional_scale(self):
        g = nv.rewrite_graph(nv.FilterGraph("scale_vaapi=1920:1080"))
        self.assertEqual(g, "scale_cuda=w=1920:h=1080")

    def test_tonemap_is_unmappable(self):
        with self.assertRaises(nv.Unmappable):
            rw(["-vf", "tonemap_vaapi=format=nv12"])

    def test_stray_vaapi_is_unmappable(self):
        with self.assertRaises(nv.Unmappable):
            rw(["-something", "vaapi"])

    def test_unknown_rc_mode(self):
        with self.assertRaises(nv.Unmappable):
            rw(["-rc_mode", "XYZ"])

    def test_quoted_comma_survives(self):
        g = nv.rewrite_graph(nv.FilterGraph("drawtext=text='a,b',scale_vaapi=w=2:h=2"))
        self.assertEqual(g, "drawtext=text='a,b',scale_cuda=w=2:h=2")

    def test_relay(self):
        url = "http://clusterplex-pms:32499/video/:/transcode/session/a/progress"
        self.assertEqual(nv.relay(url, "clusterplex-pms:32499", "10.0.0.2:32499"),
                         "http://10.0.0.2:32499/video/:/transcode/session/a/progress")


if __name__ == "__main__":
    unittest.main()
