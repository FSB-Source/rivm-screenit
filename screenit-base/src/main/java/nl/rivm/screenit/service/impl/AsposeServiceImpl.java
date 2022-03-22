package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.annotation.PostConstruct;
import javax.imageio.ImageIO;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.document.DocumentCreator;
import nl.rivm.screenit.document.VragenlijstDocumentCreator;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.UploadDocumentService;

import org.apache.commons.lang.StringUtils;
import org.ghost4j.Ghostscript;
import org.ghost4j.GhostscriptException;
import org.ghost4j.display.PageRaster;
import org.ghost4j.display.PageRasterDisplayCallback;
import org.ghost4j.document.DocumentException;
import org.ghost4j.document.PDFDocument;
import org.ghost4j.document.PSDocument;
import org.ghost4j.renderer.AbstractRemoteRenderer;
import org.ghost4j.renderer.RendererException;
import org.ghost4j.util.DiskStore;
import org.krysalis.barcode4j.impl.AbstractBarcodeBean;
import org.krysalis.barcode4j.impl.code128.Code128Bean;
import org.krysalis.barcode4j.output.bitmap.BitmapCanvasProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.aspose.words.Document;
import com.aspose.words.FieldMergingArgs;
import com.aspose.words.FontSettings;
import com.aspose.words.IFieldMergingCallback;
import com.aspose.words.ImageFieldMergingArgs;
import com.aspose.words.License;
import com.aspose.words.MergeFieldImageDimension;
import com.aspose.words.MergeFieldImageDimensionUnit;
import com.aspose.words.PdfSaveOptions;
import com.google.zxing.BarcodeFormat;
import com.google.zxing.EncodeHintType;
import com.google.zxing.WriterException;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.QRCodeWriter;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;

@Slf4j
@Service
public class AsposeServiceImpl implements AsposeService
{
	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private String asposeLicence;

	@Autowired
	private String vragenlijstTemplate;

	@Autowired
	private String locatieFilestore;

	private PdfSaveOptions pdfSaveOptions;

	public void setAsposeLicence(String asposeLicence)
	{
		this.asposeLicence = asposeLicence;
	}

	@PostConstruct
	public void init()
	{
		try
		{
			License license = new License();
			InputStream stream = new FileInputStream(asposeLicence);
			license.setLicense(stream);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
	}

	@Override
	public Document processDocument(File file, MailMergeContext context) throws Exception
	{
		Document document = null;
		try (InputStream stream = new FileInputStream(file))
		{
			document = new Document(stream);
			processDocument(document, context, true);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
		return document;
	}

	@Override
	public Document processDocument(byte[] templateDocument, MailMergeContext context) throws Exception
	{
		Document document = null;
		document = new Document(new ByteArrayInputStream(templateDocument));
		processDocument(document, context, true);
		return document;
	}

	private void processDocument(Document document, MailMergeContext context, boolean replaceMergeFieldIfNull) throws Exception
	{
		setFontsFolder();

		String veldnaam = null;
		try
		{
			document.getMailMerge().setFieldMergingCallback(new MailMergeImageCallback(context));

			Map<String, Object> mergeValues = new HashMap<>();
			for (String fieldName : document.getMailMerge().getFieldNames())
			{
				veldnaam = fieldName;
				Object afdrukObject = getAfdrukObject(fieldName, context);
				if (afdrukObject instanceof Entry)
				{
					mergeValues.put(fieldName, ((Entry<?, ?>) afdrukObject).getValue());
				}
				if (afdrukObject instanceof MergeField)
				{
					mergeValues.put(fieldName, ((MergeField) afdrukObject).getValue(context));
				}
				if (mergeValues.get(fieldName) == null && !replaceMergeFieldIfNull)
				{
					mergeValues.remove(fieldName);
				}

			}
			String[] fieldNames = mergeValues.keySet().toArray(new String[0]);
			Object[] values = mergeValues.values().toArray();
			document.getMailMerge().execute(fieldNames, values);
		}
		catch (IllegalStateException e)
		{
			LOG.error("Error creating word doc, merge fout door ", e);
			throw e;
		}
		catch (Exception e)
		{
			LOG.error("Error creating word doc, merge niet gelukt van " + veldnaam, e);
			throw e;
		}
	}

	@Override
	public Document processDocumentWithCreator(MailMergeContext context, File templateFile, DocumentCreator creator, boolean replaceMergeFieldIfNull)
	{
		Document document = null;
		try (InputStream stream = new FileInputStream(templateFile))
		{
			document = new Document(stream);
			creator.fillExecuteWithRegions(document);
			processDocument(document, context, replaceMergeFieldIfNull);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
		return document;
	}

	@Override
	public Document processVragenlijst(MailMergeContext context, ScreenitFormulierInstantie vragenlijst, boolean replaceMergeFieldIfNull) throws Exception
	{
		VragenlijstDocumentCreator creator = new VragenlijstDocumentCreator(new File(vragenlijstTemplate), vragenlijst);
		Document document = creator.getDocument();
		processDocument(document, context, replaceMergeFieldIfNull);
		return document;
	}

	@Override
	public PdfSaveOptions getPdfSaveOptions()
	{
		if (pdfSaveOptions == null)
		{
			pdfSaveOptions = new PdfSaveOptions();
			pdfSaveOptions.setUseAntiAliasing(false);
			pdfSaveOptions.setUseHighQualityRendering(true);
			pdfSaveOptions.getDownsampleOptions().setResolution(300);
		}

		return pdfSaveOptions;
	}

	private Object getAfdrukObject(String fieldName, MailMergeContext context)
	{
		MergeField mergeField = MergeField.getByFieldname(fieldName);
		if (mergeField == null && !context.getProjectAttributen().isEmpty())
		{
			for (Entry<ProjectAttribuut, String> entry : context.getProjectAttributen().entrySet())
			{
				if (fieldName.equals(entry.getKey().getMergeField()) && entry.getValue() != null)
				{
					return entry;
				}
			}
		}
		return mergeField;
	}

	private void setFontsFolder()
	{
		FontSettings.getDefaultInstance().setFontsFolder(locatieFilestore + File.separator + "fonts", false);
	}

	private static class EPSRenderer extends AbstractRemoteRenderer
	{
		public static final int OPTION_ANTIALIASING_NONE = 0;

		public static final int OPTION_ANTIALIASING_HIGH = 4;

		private int antialiasing = OPTION_ANTIALIASING_HIGH;

		private int resolution = 75;

		public EPSRenderer()
		{

			supportedDocumentClasses = new Class[2];
			supportedDocumentClasses[0] = PDFDocument.class;
			supportedDocumentClasses[1] = PSDocument.class;
		}

		@Override
		public List<PageRaster> run(org.ghost4j.document.Document document, int begin, int end) throws IOException, RendererException, org.ghost4j.document.DocumentException
		{

			this.assertDocumentSupported(document);

			Ghostscript gs = Ghostscript.getInstance();

			DiskStore diskStore = DiskStore.getInstance();
			String inputDiskStoreKey = diskStore.generateUniqueKey();

			document.write(diskStore.addFile(inputDiskStoreKey));

			PageRasterDisplayCallback displayCallback = new PageRasterDisplayCallback();

			String[] gsArgs = { "-dQUIET", "-dNOPAUSE", "-dBATCH", "-dSAFER", "-dEPSCrop", "-dFirstPage=" + (begin + 1), "-dLastPage=" + (end + 1), "-sDEVICE=display",
				"-sDisplayHandle=0", "-dDisplayFormat=16#804", "-r" + this.resolution, "-f", diskStore.getFile(inputDiskStoreKey).getAbsolutePath() };

			if (this.antialiasing != OPTION_ANTIALIASING_NONE)
			{
				gsArgs = Arrays.copyOf(gsArgs, gsArgs.length + 2);
				gsArgs[gsArgs.length - 2] = "-dTextAlphaBits=" + this.antialiasing;
				gsArgs[gsArgs.length - 1] = "-dGraphicsAlphaBits=" + this.antialiasing;
			}

			try
			{
				synchronized (gs)
				{

					gs.setDisplayCallback(displayCallback);

					gs.initialize(gsArgs);
					gs.exit();

				}
			}
			catch (GhostscriptException e)
			{

				throw new RendererException(e);

			}
			finally
			{

				try
				{
					Ghostscript.deleteInstance();
				}
				catch (GhostscriptException e)
				{
					throw new RendererException(e);
				}

				diskStore.removeFile(inputDiskStoreKey);
			}

			return displayCallback.getRasters();

		}

		public void setAntialiasing(int antialiasing)
		{
			this.antialiasing = antialiasing;
		}

		public void setResolution(int resolution)
		{
			this.resolution = resolution;
		}
	}

	private final class MailMergeImageCallback implements IFieldMergingCallback
	{
		private final MailMergeContext context;

		private MailMergeImageCallback(MailMergeContext context)
		{
			this.context = context;
		}

		@Override
		public void fieldMerging(FieldMergingArgs field)
		{

			MergeField mergeField = MergeField.getByFieldname(field.getFieldName());
			if (field.getFieldValue() instanceof String && mergeField != null && !mergeField.waardeTrimmen())
			{
				field.setText((String) field.getFieldValue());
			}
		}

		@Override
		public void imageFieldMerging(ImageFieldMergingArgs field)
			throws IOException, InstantiationException, IllegalAccessException, RendererException, DocumentException, NoSuchMethodException, InvocationTargetException
		{

			Object afdrukObject = getAfdrukObject(field.getFieldName(), context);
			if (afdrukObject instanceof Entry)
			{
				Entry<ProjectAttribuut, String> entry = (Entry<ProjectAttribuut, String>) afdrukObject;
				if (entry.getKey().isBarcode())
				{
					barcodeMerger(field, (entry).getValue(), null, new Code128Bean());
				}
			}
			if (afdrukObject instanceof MergeField)
			{
				MergeField mergeField = (MergeField) afdrukObject;
				Object mergeFieldValue = mergeField.getValue(context);

				if (mergeFieldValue == null)
				{
					throw new IllegalStateException("MergeField " + mergeField + " heeft geen waarde");
				}
				if (mergeField.isBarcode())
				{
					AbstractBarcodeBean abstractBarcodeBean = mergeField.getBarcodeType().getConstructor().newInstance();
					barcodeMerger(field, mergeFieldValue.toString(), mergeField.getBarcodeHeight(), abstractBarcodeBean);
				}
				else if (mergeField.isQRcode())
				{
					qrcodeMerger(field, mergeFieldValue.toString());
				}
				else if (mergeFieldValue instanceof UploadDocument)
				{
					imageMerger(field, (UploadDocument) mergeFieldValue);
				}
			}
		}

		private void imageMerger(ImageFieldMergingArgs field, UploadDocument uploadDocument) throws IOException, RendererException, DocumentException
		{
			File mergeFieldFile = uploadDocumentService.load(uploadDocument);
			if (uploadDocument.getContentType().equals("image/x-eps") || uploadDocument.getContentType().equals("application/postscript"))
			{
				PSDocument document = new PSDocument();
				document.load(mergeFieldFile);
				EPSRenderer renderer = new EPSRenderer();
				renderer.setResolution(100);

				List<Image> images = renderer.render(document);
				BufferedImage image = (BufferedImage) images.get(0);

				File png = File.createTempFile("image", ".png");
				LOG.info("png image: " + png);
				ImageIO.write(image, "png", png);
				field.setImage(image);
				field.setImageHeight(new MergeFieldImageDimension(40, MergeFieldImageDimensionUnit.POINT));
				field.setImageWidth(new MergeFieldImageDimension(160, MergeFieldImageDimensionUnit.POINT));
			}
			else
			{
				try (InputStream inputStream = new FileInputStream(mergeFieldFile))
				{
					field.setImageStream(inputStream);
				}
				catch (Exception e)
				{
					LOG.error(e.getMessage(), e);
				}
			}

		}

		private void qrcodeMerger(ImageFieldMergingArgs field, String message)
		{

			ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

			if (StringUtils.isNotBlank(message))
			{
				message = Constants.LOCATIEID + "=" + message;

				int size = 100;
				String fileType = "png";
				try
				{
					Map<EncodeHintType, ErrorCorrectionLevel> hintMap = new HashMap<>();
					hintMap.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.L);
					QRCodeWriter qrCodeWriter = new QRCodeWriter();
					BitMatrix byteMatrix = qrCodeWriter.encode(message, BarcodeFormat.QR_CODE, size, size, hintMap);
					int crunchifyWidth = byteMatrix.getWidth();
					BufferedImage image = new BufferedImage(crunchifyWidth, crunchifyWidth, BufferedImage.TYPE_INT_RGB);
					image.createGraphics();

					Graphics2D graphics = (Graphics2D) image.getGraphics();
					graphics.setColor(Color.WHITE);
					graphics.fillRect(0, 0, crunchifyWidth, crunchifyWidth);
					graphics.setColor(Color.BLACK);

					for (int x = 0; x < crunchifyWidth; x++)
					{
						for (int y = 0; y < crunchifyWidth; y++)
						{
							if (byteMatrix.get(x, y))
							{
								graphics.fillRect(x, y, 1, 1);
							}
						}
					}
					ImageIO.write(image, fileType, outputStream);
				}
				catch (WriterException | IOException e)
				{
					LOG.error(e.getMessage());
				}
			}

			InputStream inputStream = new ByteArrayInputStream(outputStream.toByteArray());
			field.setImageStream(inputStream);
		}

		private void barcodeMerger(ImageFieldMergingArgs field, String message, Double barcodeHeight, AbstractBarcodeBean barcodeBean) throws IOException
		{

			ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
			if (StringUtils.isNotBlank(message))
			{
				BitmapCanvasProvider canvas = new BitmapCanvasProvider(outputStream, "image/x-png", 300, BufferedImage.TYPE_BYTE_BINARY, false, 0);
				if (barcodeBean == null)
				{
					barcodeBean = new Code128Bean();
				}
				if (barcodeHeight != null)
				{
					barcodeBean.setBarHeight(barcodeHeight);
				}
				try
				{
					barcodeBean.generateBarcode(canvas, message);
					canvas.finish();
				}
				catch (IllegalArgumentException iae)
				{
					LOG.error(iae.getMessage(), iae);
				}
			}

			InputStream inputStream = new ByteArrayInputStream(outputStream.toByteArray());
			field.setImageStream(inputStream);
		}
	}
}
