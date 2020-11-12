
package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.annotation.PostConstruct;
import javax.imageio.ImageIO;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.document.DocumentCreator;
import nl.rivm.screenit.document.VragenlijstDocumentCreator;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.FileService;

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
import org.krysalis.barcode4j.output.bitmap.BitmapCanvasProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

@Service
public class AsposeServiceImpl implements AsposeService
{
	
	private static final Logger LOG = LoggerFactory.getLogger(AsposeServiceImpl.class);

	@Autowired
	private FileService fileService;

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
		try (InputStream stream = new FileInputStream(file);)
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

	private Document processDocument(Document document, MailMergeContext context, boolean replaceMergeFieldIfNull) throws Exception
	{
		setFontsFolder();

		String mergeVeldNaam = null;
		try
		{
			document.getMailMerge().setFieldMergingCallback(new MailMergeImageCallback(context));

			Map<String, Object> mergeValues = new HashMap<>();
			for (String fieldName : document.getMailMerge().getFieldNames())
			{
				mergeVeldNaam = fieldName;
				Object mergeFieldValue = getMergeFieldValue(fieldName, context);
				if (mergeFieldValue != null || replaceMergeFieldIfNull)
				{
					mergeValues.put(fieldName, mergeFieldValue);
				}
			}
			document.getMailMerge().execute(mergeValues.keySet().toArray(new String[mergeValues.size()]), mergeValues.values().toArray());
		}
		catch (Exception e)
		{
			LOG.error("Error creating word doc, merge niet gelukt van " + mergeVeldNaam, e);
			throw e;
		}
		return document;
	}

	@Override
	public Document processDocumentWithCreator(MailMergeContext context, File templateFile, DocumentCreator creator, boolean replaceMergeFieldIfNull)
	{
		Document document = null;
		try (InputStream stream = new FileInputStream(templateFile);)
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

	private Object getMergeFieldValue(String fieldName, MailMergeContext context)
	{
		MergeField mergeField = MergeField.getByFieldname(fieldName);
		if (mergeField == null && !context.getProjectAttributen().isEmpty())
		{
			for (Entry<ProjectAttribuut, String> entry : context.getProjectAttributen().entrySet())
			{
				if (fieldName.equals(entry.getKey().getMergeField()) && entry.getValue() != null)
				{
					return entry.getValue();
				}
			}
		}
		if (mergeField == null)
		{
			return null;
		}
		return mergeField.getValue(context);
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

	final private class MailMergeImageCallback implements IFieldMergingCallback
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
		public void imageFieldMerging(ImageFieldMergingArgs field) throws IOException, InstantiationException, IllegalAccessException, RendererException, DocumentException
		{
			final int dpi = 300;
			if (MergeField.getByFieldname(field.getFieldName()) == null)
			{
				return;
			}
			if (getMergeFieldValue(field.getFieldName(), context) == null)
			{
				return;
			}

			if (MergeField.getByFieldname(field.getFieldName()).isBarcode())
			{

				ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

				String msg = getMergeFieldValue(field.getFieldName(), context).toString();

				if (StringUtils.isNotBlank(msg))
				{
					BitmapCanvasProvider canvas = new BitmapCanvasProvider(outputStream, "image/x-png", dpi, BufferedImage.TYPE_BYTE_BINARY, false, 0);
					AbstractBarcodeBean barcodeBean = getBarcodeBean(field.getFieldName());
					try
					{
						barcodeBean.generateBarcode(canvas, msg);
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
			else if (MergeField.getByFieldname(field.getFieldName()).isQRcode())
			{
				String msg = getMergeFieldValue(field.getFieldName(), context).toString();

				ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

				if (StringUtils.isNotBlank(msg))
				{
					msg = Constants.LOCATIEID + "=" + msg;

					int size = 100;
					String fileType = "png";
					try
					{
						Map<EncodeHintType, ErrorCorrectionLevel> hintMap = new Hashtable<>();
						hintMap.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.L);
						QRCodeWriter qrCodeWriter = new QRCodeWriter();
						BitMatrix byteMatrix = qrCodeWriter.encode(msg, BarcodeFormat.QR_CODE, size, size, hintMap);
						int crunchifyWidth = byteMatrix.getWidth();
						BufferedImage image = new BufferedImage(crunchifyWidth, crunchifyWidth, BufferedImage.TYPE_INT_RGB);
						image.createGraphics();

						Graphics2D graphics = (Graphics2D) image.getGraphics();
						graphics.setColor(Color.WHITE);
						graphics.fillRect(0, 0, crunchifyWidth, crunchifyWidth);
						graphics.setColor(Color.BLACK);

						for (int i = 0; i < crunchifyWidth; i++)
						{
							for (int j = 0; j < crunchifyWidth; j++)
							{
								if (byteMatrix.get(i, j))
								{
									graphics.fillRect(i, j, 1, 1);
								}
							}
						}
						ImageIO.write(image, fileType, outputStream);
					}
					catch (WriterException e)
					{
						LOG.error(e.getMessage());
					}
					catch (IOException e)
					{
						LOG.error(e.getMessage());
					}
				}

				InputStream inputStream = new ByteArrayInputStream(outputStream.toByteArray());
				field.setImageStream(inputStream);
			}
			else
			{
				Object mergeFieldValue = MergeField.getByFieldname(field.getFieldName()).getValue(context);
				if (mergeFieldValue instanceof UploadDocument)
				{
					UploadDocument uploadDocument = (UploadDocument) mergeFieldValue;
					File mergeFieldFile = fileService.load(uploadDocument);

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
						InputStream inputStream = null;
						try
						{
							inputStream = new FileInputStream(mergeFieldFile);
							field.setImageStream(inputStream);
						}
						catch (Exception e)
						{
							LOG.error(e.getMessage(), e);
						}
						finally
						{
							if (inputStream != null)
							{
								inputStream.close();
							}
						}
					}
				}
			}
		}

		private AbstractBarcodeBean getBarcodeBean(String fieldName) throws InstantiationException, IllegalAccessException
		{
			MergeField mergeField = MergeField.getByFieldname(fieldName);
			Double mergeFieldBarcodeHeight = mergeField.getBarcodeHeight();
			AbstractBarcodeBean newInstance = mergeField.getBarcodeType().newInstance();
			if (mergeFieldBarcodeHeight != null)
			{
				newInstance.setBarHeight(mergeFieldBarcodeHeight);
			}
			return newInstance;
		}
	}

}
