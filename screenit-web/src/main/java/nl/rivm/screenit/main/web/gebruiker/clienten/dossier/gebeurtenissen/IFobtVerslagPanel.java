package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.List;

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.service.colon.ColonDossierService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.ClientDossierPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.util.IFOBTTestUtil;
import nl.topicuszorg.documentupload.wicket.UploadDocumentLink;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_CLIENT_SR_UITSLAGIFOBTONTVANGEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class IFobtVerslagPanel extends AbstractGebeurtenisDetailPanel
{

	private BootstrapDialog confirmDialog;

	@SpringBean
	private ColonDossierService colonDossierService;

	@SpringBean
	private FileService fileService;

	private IModel<List<FileUpload>> file = new ListModel<>();

	private FileUploadField uploadField;

	private UploadDocument uploadDocument;

	private IndicatingAjaxSubmitLink formUploadBtn;

	private File tmpFile;

	private Form uploadForm;

	private static final Logger LOG = LoggerFactory.getLogger(IFobtVerslagPanel.class);

	public IFobtVerslagPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
		uploadForm = new Form<>("uploadForm", model);
		uploadForm.setOutputMarkupId(true);
		uploadForm.setOutputMarkupPlaceholderTag(true);
		add(uploadForm);

		uploadForm.add(DateLabel.forDatePattern("buis.statusDatum", "dd-MM-yyyy"));
		uploadForm.add(DateLabel.forDatePattern("buis.afnameDatum", "dd-MM-yyyy"));
		uploadForm.add(DateLabel.forDatePattern("buis.analyseDatum", "dd-MM-yyyy"));
		uploadForm.add(new Label("buis.instumentId"));
		uploadForm.add(new Label("buis.ifobtLaboratorium.naam"));
		uploadForm.add(new EnumLabel<IFOBTType>("buis.type"));

		IFOBTTest buis = getModelObject().getBuis();
		BigDecimal uitslag = null;
		IFOBTTestStatus status = null;

		if (buis != null)
		{
			status = buis.getStatus();
			uitslag = buis.getUitslag();
			if (uitslag != null)
			{
				uploadForm.add(new Label("uitslag", uitslag + ""));
			}
		}
		if (uitslag == null)
		{
			uploadForm.add(new Label("uitslag", ""));
		}

		String interpretatie = getInterpretatie(buis, status);
		uploadForm.add(new Label("interpretatie", interpretatie));
		if (status != null)
		{
			uploadForm.add(new EnumLabel<IFOBTTestStatus>("buis.status"));
		}
		else
		{
			uploadForm.add(new Label("buis.status", ""));
		}
		uploadForm.add(new Label("buis.barcode"));

		createUploadField(status);
		createVervangenUploadSubmitBtn();

		confirmDialog = new BootstrapDialog("confirmDialog");
		add(confirmDialog);
	}

	private void createVervangenUploadSubmitBtn()
	{
		formUploadBtn = new IndicatingAjaxSubmitLink("uploadSubmitBtn")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (file.getObject().size() == 1)
				{
					FileUpload fileUpload = file.getObject().get(0);
					try
					{
						tmpFile = fileUpload.writeToTempFile();
						tmpFile.deleteOnExit();
						maakUploadDocument(fileUpload);
						uploadDocument.setFile(tmpFile);
						Client client = getModelObject().getBuis().getColonScreeningRonde().getDossier().getClient();
						fileService.saveOrUpdateUploadDocument(uploadDocument, FileStoreLocation.COLON_UITSLAG_VERWIJDEREN_CLIENT_BRIEF, client.getId());
						colonDossierService.vervangUitslagVerwijderenDocument(getModelObject().getBuis(), uploadDocument);
						setResponsePage(new ClientDossierPage(ModelUtil.sModel(client)));
					}
					catch (Exception e)
					{
						LOG.error("Fout bij uploaden van een formulier: ", e);
						error(getString("error.onbekend"));
					}
				}
				else
				{
					LOG.error("Er mag maar 1 bestand geuploaded worden als formulier");
					error(getString("error.onjuistaantalfiles"));
				}
			}
		};
		formUploadBtn.setOutputMarkupId(true);
		formUploadBtn.setOutputMarkupPlaceholderTag(true);
		formUploadBtn.setVisible(false);
		uploadForm.add(formUploadBtn);
	}

	private void createUploadField(IFOBTTestStatus status)
	{
		uploadField = new FileUploadField("fileUpload", file);
		uploadField.add(new FileValidator(FileType.PDF));
		uploadField.setRequired(true);
		uploadField.setOutputMarkupId(true);
		uploadField.setOutputMarkupPlaceholderTag(true);
		uploadField.setVisible(magVerwijderen());
		uploadForm.add(uploadField);
	}

	@Override
	protected void addButton(String id, GebeurtenisPopupBasePanel parent)
	{
		ConfirmingIndicatingAjaxSubmitLink<Void> button = new ConfirmingIndicatingAjaxSubmitLink<Void>(id, uploadForm, confirmDialog, "label.ifobtuitslag.verwijderen")
		{
			@Override
			protected boolean skipConfirmation()
			{
				if (file.getObject().size() == 1)
				{
					FileUpload fileUpload = file.getObject().get(0);
					try
					{
						tmpFile = fileUpload.writeToTempFile();
						tmpFile.deleteOnExit();
						maakUploadDocument(fileUpload);
					}
					catch (Exception e)
					{
						LOG.error("Fout bij uploaden van een formulier: ", e);
						error(getString("error.onbekend"));
						return true;
					}
				}
				else
				{
					LOG.error("Er mag maar 1 bestand geuploaded worden als formulier");
					error(getString("error.onjuistaantalfiles"));
					return true;
				}
				return super.skipConfirmation();
			}

			@Override
			public void onNoClick(AjaxRequestTarget target)
			{
				super.onNoClick(target);
				tmpFile.delete();
			}

			@Override
			public void onCloseClick(AjaxRequestTarget target)
			{
				super.onCloseClick(target);
				tmpFile.delete();
			}

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{

				IFOBTTest buis = IFobtVerslagPanel.this.getModelObject().getBuis();
				ColonUitnodiging uitnodiging = (ColonUitnodiging) IFobtVerslagPanel.this.getModelObject().getUitnodiging();
				try
				{
					uploadDocument.setFile(tmpFile);
					fileService.saveOrUpdateUploadDocument(uploadDocument, FileStoreLocation.COLON_UITSLAG_VERWIJDEREN_CLIENT_BRIEF,
						uitnodiging.getScreeningRonde().getDossier().getClient().getId());
				}
				catch (IOException e)
				{
					LOG.error("Fout bij uploaden van een bezwaar formulier: ", e);
					error(getString("error.onbekend"));
				}
				colonDossierService.verwijderIfobtUitslag(buis, uploadDocument, ScreenitSession.get().getLoggedInInstellingGebruiker());
				ScreenitSession.get().info(IFobtVerslagPanel.this.getString("ifobtuitslag.verwijderd"));

				setResponsePage(new ClientDossierPage(ModelUtil.sModel(uitnodiging.getScreeningRonde().getDossier().getClient())));
			}
		};
		button.add(new Label("label", getString("label.verwijderen")));
		button.add(new AttributeAppender("class", Model.of(" btn-danger")));
		button.setVisible(magVerwijderen());
		parent.add(button);
	}

	private boolean magVerwijderen()
	{
		boolean magVerwijderen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_UITSLAGIFOBTONTVANGEN, Actie.VERWIJDEREN);
		ScreeningRondeGebeurtenis screeningRondeGebeurtenis = getModelObject();
		ColonUitnodiging uitnodiging = (ColonUitnodiging) screeningRondeGebeurtenis.getUitnodiging();
		if (uitnodiging != null)
		{
			ColonDossier dossier = uitnodiging.getScreeningRonde().getDossier();
			IFOBTTest buis = screeningRondeGebeurtenis.getBuis();
			if (!IFOBTTestUtil.isLaatsteUitslagVanLaatsteRonde(dossier, uitnodiging.getGekoppeldeTest().getStatusDatum()))
			{
				magVerwijderen = false;
			}
			else if (dossier.getLaatsteScreeningRonde().getLaatsteAfspraak() != null && dossier.getLaatsteScreeningRonde().getLaatsteAfspraak().getConclusie() != null)
			{

				magVerwijderen = false;
			}
			else if (buis != null)
			{
				IFOBTTestStatus testStatus = buis.getStatus();
				if (IFOBTTestStatus.VERWIJDERD.equals(testStatus) || buis.getType().equals(IFOBTType.GOLD) && buis.getUitslag() == null)
				{

					magVerwijderen = false;
				}
			}
		}
		return magVerwijderen;
	}

	private void maakUploadDocument(FileUpload fileUpload)
	{
		uploadDocument = new UploadDocument();
		uploadDocument.setActief(Boolean.TRUE);
		uploadDocument.setContentType(fileUpload.getContentType());
		uploadDocument.setNaam(fileUpload.getClientFileName());
	}

	private String getInterpretatie(IFOBTTest buis, IFOBTTestStatus status)
	{
		String interpretatie;
		if (IFOBTTestStatus.VERWIJDERD.equals(status))
		{
			interpretatie = "Verwijderd";
		}
		else if (IFOBTTestUtil.isGunstig(buis))
		{
			interpretatie = "Gunstig";
		}
		else if (IFOBTTestUtil.isOngunstig(buis))
		{
			interpretatie = "Ongunstig";
		}
		else if (buis.getUitslag() != null && buis.getNormWaarde() == null)
		{
			interpretatie = "";
		}
		else
		{
			interpretatie = "Geen uitslag";
		}
		return interpretatie;
	}

	@Override
	protected void addDocumentVervangenButton(String id, GebeurtenisPopupBasePanel parent)
	{
		IndicatingAjaxLink btn = new IndicatingAjaxLink<Void>(id)
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				uploadField.setVisible(true);
				formUploadBtn.setVisible(IFOBTTestStatus.VERWIJDERD.equals(IFobtVerslagPanel.this.getModelObject().getBuis().getStatus()));
				target.add(uploadForm);
			}
		};
		btn.setVisible(IFOBTTestStatus.VERWIJDERD.equals(getModelObject().getBuis().getStatus()));
		parent.add(btn);
	}

	@Override
	protected void addDocumentDownloadenButton(String id, GebeurtenisPopupBasePanel parent)
	{
		UploadDocumentLink briefDownloadBtn = new UploadDocumentLink(id, new PropertyModel<>(getModel(), "buis.verwijderbrief"), true);
		briefDownloadBtn.setVisible(IFOBTTestStatus.VERWIJDERD.equals(getModelObject().getBuis().getStatus()));
		parent.add(briefDownloadBtn);
	}
}
