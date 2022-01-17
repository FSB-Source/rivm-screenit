package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.io.IOException;
import java.util.List;

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.ClientDossierPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.GebeurtenisPopupBasePanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.IFobtVerslagPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.cervix.CervixBaseUitnodigingService;
import nl.topicuszorg.documentupload.wicket.UploadDocumentLink;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
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
	recht = Recht.GEBRUIKER_CLIENT_SR_CERVIX_HPV_UITSLAG_INZIEN,
	bevolkingsonderzoekScopes = Bevolkingsonderzoek.CERVIX)
public class CervixHpvInzienPanel extends AbstractGebeurtenisDetailPanel
{
	private static final Logger LOG = LoggerFactory.getLogger(IFobtVerslagPanel.class);

	@SpringBean
	private CervixBaseUitnodigingService cervixUitnodigingService;

	@SpringBean
	private FileService fileService;

	private BootstrapDialog confirmDialog;

	private IModel<List<FileUpload>> file = new ListModel<>();

	private FileUploadField uploadField;

	private UploadDocument uploadDocument;

	private File tmpFile;

	private Form uploadForm;

	private IndicatingAjaxSubmitLink formUploadBtn;

	public CervixHpvInzienPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		uploadForm = new Form<>("uploadForm", getModel());
		uploadForm.setOutputMarkupId(true);
		uploadForm.setOutputMarkupPlaceholderTag(true);
		add(uploadForm);

		uploadForm.add(new Label("uitnodiging.monster.monsterId"));
		uploadForm.add(DateLabel.forDatePattern("beoordeling.analyseDatum", "dd-MM-yyyy"));
		uploadForm.add(DateLabel.forDatePattern("beoordeling.autorisatieDatum", "dd-MM-yyyy"));
		uploadForm.add(new Label("beoordeling.hpvUitslag.naam"));
		uploadForm.add(new Label("uitnodiging.monster.laatsteHpvBeoordeling.hpvBericht.laboratorium.naam"));
		uploadForm.add(new Label("beoordeling.hpvBericht.messageId"));
		uploadForm.add(new Label("beoordeling.hpvBericht.instrumentId"));
		confirmDialog = new BootstrapDialog("confirmDialog");
		add(confirmDialog);

		createUploadField();
		createVervangenUploadSubmitBtn();
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
						Client client = getModelObject().getBeoordeling().getMonster().getBrief().getClient();
						fileService.saveOrUpdateUploadDocument(uploadDocument, FileStoreLocation.CERVIX_UITSLAG_VERWIJDEREN_CLIENT_BRIEF, client.getId());
						cervixUitnodigingService.vervangVerwijderdDocument(getModelObject().getBeoordeling().getMonster(), uploadDocument);
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

	private void createUploadField()
	{
		uploadField = new FileUploadField("fileUpload", file);
		uploadField.add(new FileValidator(FileType.PDF));
		uploadField.setRequired(true);
		uploadField.setVisible(getModelObject().getBeoordeling().getMonster().getVerwijderdDatum() == null && magVerwijderen());
		uploadForm.add(uploadField);
	}

	@Override
	protected void addButton(String id, GebeurtenisPopupBasePanel parent)
	{
		ConfirmingIndicatingAjaxSubmitLink<Void> button = new ConfirmingIndicatingAjaxSubmitLink<>(id, uploadForm, confirmDialog, "label.resultaten.monster.verwijderen")
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
				CervixUitnodiging uitnodiging = (CervixUitnodiging) CervixHpvInzienPanel.this.getModelObject().getUitnodiging();
				try
				{
					uploadDocument.setFile(tmpFile);
					fileService.saveOrUpdateUploadDocument(uploadDocument, FileStoreLocation.CERVIX_UITSLAG_VERWIJDEREN_CLIENT_BRIEF,
						uitnodiging.getScreeningRonde().getDossier().getClient().getId());
				}
				catch (IOException e)
				{
					LOG.error("Fout bij uploaden van een bezwaar formulier: ", e);
					error(getString("error.onbekend"));
				}
				cervixUitnodigingService.verwijderResultatenMonster(uitnodiging.getMonster(), uploadDocument, ScreenitSession.get().getLoggedInInstellingGebruiker());
				ScreenitSession.get().info(CervixHpvInzienPanel.this.getString("uitslagen.verwijderd"));
				setResponsePage(new ClientDossierPage(ModelUtil.sModel(uitnodiging.getScreeningRonde().getDossier().getClient())));
			}
		};

		button.add(new Label("label", getString("label.verwijderen")));
		button.add(new AttributeAppender("class", Model.of(" left btn-danger")));
		button.setVisible(magVerwijderen());
		parent.add(button);
	}

	private boolean magVerwijderen()
	{
		CervixUitnodiging uitnodiging = (CervixUitnodiging) getModelObject().getUitnodiging();
		CervixMonster monster = uitnodiging.getMonster();
		CervixScreeningRonde ontvangstRonde = monster.getOntvangstScreeningRonde();
		return ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_VERWIJDEREN_RESULTATEN_MONSTER, Actie.VERWIJDEREN, ontvangstRonde.getDossier().getClient())
			&& monster.equals(cervixUitnodigingService.getUitnodigingMagVerwijderdWorden(ontvangstRonde));
	}

	private void maakUploadDocument(FileUpload fileUpload)
	{
		uploadDocument = new UploadDocument();
		uploadDocument.setActief(Boolean.TRUE);
		uploadDocument.setContentType(fileUpload.getContentType());
		uploadDocument.setNaam(fileUpload.getClientFileName());
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
				formUploadBtn.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_VERWIJDEREN_RESULTATEN_MONSTER, Actie.VERWIJDEREN,
					CervixHpvInzienPanel.this.getModelObject().getUitnodiging().getScreeningRonde().getDossier().getClient())
					&& CervixHpvInzienPanel.this.getModelObject().getBeoordeling().getMonster().getVerwijderdBrief() != null);
				target.add(uploadForm);
			}
		};
		btn.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_VERWIJDEREN_RESULTATEN_MONSTER, Actie.VERWIJDEREN,
			getModelObject().getUitnodiging().getScreeningRonde().getDossier().getClient())
			&& CervixHpvInzienPanel.this.getModelObject().getBeoordeling().getMonster().getVerwijderdBrief() != null);
		parent.add(btn);
	}

	@Override
	protected void addDocumentDownloadenButton(String id, GebeurtenisPopupBasePanel parent)
	{
		UploadDocumentLink briefDownloadBtn = new UploadDocumentLink(id, new PropertyModel<>(getModel(), "beoordeling.monster.verwijderdBrief"), true);
		briefDownloadBtn.setVisible(getModelObject().getBeoordeling().getMonster().getVerwijderdDatum() != null);
		parent.add(briefDownloadBtn);
	}
}
