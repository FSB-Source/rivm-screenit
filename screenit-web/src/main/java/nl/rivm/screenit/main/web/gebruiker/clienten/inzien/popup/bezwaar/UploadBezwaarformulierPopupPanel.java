package nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.bezwaar;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.comparator.BriefCreatieDatumComparator;
import nl.rivm.screenit.main.service.BriefService;
import nl.rivm.screenit.main.util.BriefOmschrijvingUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.bezwaar.edit.BezwaarEditPanel;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientInzienBezwaarPanel;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.algemeen.BezwaarViewWrapper;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.poi.ss.formula.functions.T;
import org.apache.shiro.util.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class UploadBezwaarformulierPopupPanel extends GenericPanel<BezwaarMoment>
{

	private static final Logger LOG = LoggerFactory.getLogger(UploadBezwaarformulierPopupPanel.class);

	private static final long serialVersionUID = 1L;

	@SpringBean
	private LogService logService;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private BriefService briefService;

	@SpringBean
	private BezwaarService bezwaarService;

	@SpringBean
	private FileService fileService;

	private ClientInzienBezwaarPanel clientInzienBezwaarPanel;

	final private BootstrapDialog dialog;

	private IModel<BezwaarMoment> bezwaarModel;

	private List<BezwaarGroupViewWrapper> bezwaarViewModels;

	private IModel<List<FileUpload>> files = new ListModel<>();

	private IModel<UploadDocument> document;

	private Form<BezwaarMoment> uploadForm;

	public UploadBezwaarformulierPopupPanel(String id, IModel<BezwaarMoment> model, ClientInzienBezwaarPanel clientInzienBezwaarPanel,
		BootstrapDialog dialog)
	{
		super(id, new CompoundPropertyModel<BezwaarMoment>(model));
		this.dialog = dialog;
		this.clientInzienBezwaarPanel = clientInzienBezwaarPanel;
		this.bezwaarModel = model;
		this.uploadForm = new ScreenitForm<>("uploadForm", bezwaarModel);
		add(uploadForm);

		uploadForm.add(
			new FileUploadField("fileUpload", files).add(new FileValidator(FileType.PDF)).setRequired(true));

		uploadForm.add(
			new ListView<String>("brievenLijst",
				BriefOmschrijvingUtil.getBrievenOmschrijvingen(briefService.getBrievenVanBezwaar(model.getObject()), this::getString))
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void populateItem(ListItem<String> item)
				{
					String tekst = item.getModelObject();
					item.add(new Label("brief", Model.<String> of(tekst)));

				}

			});

		Client client = model.getObject().getClient();
		bezwaarViewModels = bezwaarService.getEditBezwaarGroupViewWrappers(client, client.getLaatstVoltooideBezwaarMoment());
		uploadForm.add(new BezwaarEditPanel("bezwaarEditPanel", bezwaarViewModels, true));
		addButtons();
	}

	private void addButtons()
	{
		uploadForm.add(new AjaxLink<T>("geenHandtekening")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				bezwaarService.nogmaalsVersturen(bezwaarModel.getObject(), ScreenitSession.get().getLoggedInAccount());
				info(getString("info.bezwaargeenhandtekening"));
				close(target);
			}

			@Override
			public boolean isVisible()
			{
				return BriefType.CLIENT_BEZWAAR_AANVRAAG.equals(bezwaarModel.getObject().getBezwaarAanvraag().getBriefType());
			}
		});

		uploadForm.add(new AjaxLink<T>("nogmaalsVersturen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				BezwaarBrief brief = getOrgineleLaatsteBrief();
				briefService.opnieuwAanmaken(brief, ScreenitSession.get().getLoggedInAccount());
				info(getString("info.bezwaarnogmaalsverstuurd"));
				close(target);
			}
		});

		BezwaarBrief laatsteBrief = getLaatsteBrief();

		boolean magTegenhouden = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN, Actie.AANPASSEN);
		uploadForm.add(new AjaxLink<T>("tegenhouden")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				BezwaarBrief brief = getLaatsteBrief();
				bezwaarService.algemeneBezwaarBriefTegenhouden(brief, ScreenitSession.get().getLoggedInAccount());
				info(getString("info.brieftegenhouden"));
				close(target);
			}
		}.setVisible(
			magTegenhouden && laatsteBrief != null && !laatsteBrief.isTegenhouden() && laatsteBrief.getMergedBrieven() == null));

		uploadForm.add(new AjaxLink<T>("doorvoeren")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				BezwaarBrief brief = getLaatsteBrief();
				bezwaarService.algemeneBezwaarBriefDoorvoeren(brief, ScreenitSession.get().getLoggedInAccount());
				info(getString("info.briefactiveren"));
				close(target);
			}
		}.setVisible(magTegenhouden && laatsteBrief != null && laatsteBrief.isTegenhouden()));

		uploadForm.add(new AjaxSubmitLink("doorgaan")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				BezwaarMoment bezwaar = bezwaarModel.getObject();
				boolean bezwaarBRP = isErEenBezwaarMet(BezwaarType.GEEN_OPNAME_UIT_BPR);
				boolean bezwaarVerwijderenClientDossier = isErEenBezwaarMet(BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER);
				boolean meldingen = false;
				if (bezwaarVerwijderenClientDossier && !heeftRechtVoor(Recht.CLIENT_DOSSIER_VERWIJDEREN))
				{
					LOG.info("Client Dossier kan niet worden verwijderd vanwege rechten. client(id: " + bezwaar.getClient().getId() + ")");
					error(getString("error.recht.dossierverwijderen"));
					meldingen = true;
				}
				if (bezwaarBRP && !heeftRechtVoor(Recht.GEBRUIKER_BEZWAAR_BRP))
				{
					LOG.info("Bezwaar BRP kan niet worden ingediend vanwege rechten. client(id: " + bezwaar.getClient().getId() + ")");
					error(getString("error.recht.bezwaarbrp"));
					meldingen = true;
				}
				if (meldingen)
				{
					return;
				}
				if (files.getObject().size() != 1)
				{
					LOG.error(getString("error.een.bestand.uploaden.bezwaarformulier"));
					error(getString("error.een.bestand.uploaden.bezwaarformulier"));
					return;
				}
				if (!bezwarenGewijzigd())
				{
					error(getString("error.bezwaar.niet.gewijzigd"));
					return;
				}

				stelFileVeilig();
				if (bezwaarVerwijderenClientDossier && heeftRechtVoor(Recht.CLIENT_DOSSIER_VERWIJDEREN)
					|| bezwaarBRP && heeftRechtVoor(Recht.GEBRUIKER_BEZWAAR_BRP))
				{
					dialog.close(target);
					dialog.openWith(target,
						new BevestigVerwijderClientDossierPopupPanel(IDialog.CONTENT_ID, bezwaarBRP, bezwaarVerwijderenClientDossier)
						{
							private static final long serialVersionUID = 1L;

							@Override
							protected void opslaan(AjaxRequestTarget target)
							{
								saveBezwaar(target, true);
							}
						});
				}
				else
				{
					saveBezwaar(target, false);
				}
			}

			private boolean bezwarenGewijzigd()
			{
				BezwaarMoment laatstVoltooideBezwaarMoment = bezwaarModel.getObject().getClient().getLaatstVoltooideBezwaarMoment();
				return bezwaarService.bezwarenGewijzigd(laatstVoltooideBezwaarMoment, bezwaarViewModels, null);
			}
		});
	}

	private BezwaarBrief getOrgineleLaatsteBrief()
	{
		BezwaarBrief brief = null;
		BezwaarMoment moment = getModelObject();
		if (moment.getBezwaarAanvraag() != null)
		{
			brief = moment.getBezwaarAanvraag();
		}
		return brief;
	}

	private BezwaarBrief getLaatsteBrief()
	{
		List<BezwaarBrief> brieven = briefService.getBrievenVanBezwaar(getModelObject());
		Collections.sort(brieven, new BriefCreatieDatumComparator().reversed());
		if (!CollectionUtils.isEmpty(brieven))
		{
			return brieven.get(0);
		}
		return null;
	}

	private void stelFileVeilig()
	{
		try
		{
			if (files.getObject() != null)
			{
				FileUpload upload = files.getObject().get(0);
				File definitieFile = upload.writeToTempFile();

				UploadDocument document = new UploadDocument();
				document.setActief(Boolean.TRUE);
				document.setContentType(upload.getContentType());
				document.setFile(definitieFile);
				document.setNaam(upload.getClientFileName());
				this.document = ModelUtil.cModel(document);
			}
		}
		catch (Exception e)
		{
			LOG.error("Fout bij uploaden van een bezwaaruploaden naar tmp directory: ", e);
			error(getString("error.onbekend"));
			return;
		}
	}

	private void saveBezwaar(AjaxRequestTarget target, boolean verwijderenDossier)
	{
		try
		{
			List<BezwaarGroupViewWrapper> bezwaarWrappers = bezwaarViewModels;
			BezwaarMoment moment = bezwaarModel.getObject();

			UploadDocument document = this.document.getObject();
			fileService.saveOrUpdateUploadDocument(document, FileStoreLocation.BEZWAAR, moment.getClient().getId());
			moment.setBezwaarBrief(document);
			hibernateService.saveOrUpdate(moment);

			bezwaarService.bezwaarAfronden(moment, ScreenitSession.get().getLoggedInAccount(), bezwaarWrappers);
			getClientInzienBezwaarPanel().verversMeldingenNaAanpassingBezwaar(bezwaarModel, target);

			String melding = "Bezwaar is aangepast. ";
			if (verwijderenDossier)
			{
				melding += "Dossier is succesvol verwijderd!";
			}
			info(melding);
		}
		catch (IOException e)
		{
			LOG.error("Fout bij verwerken van het geuploade bestand op de fileserver: ", e);
			error(getString("error.onbekend"));
			return;
		}
		catch (Exception e)
		{
			LOG.error("Er heeft zich een onbekende fout voorgedaan met het verwerken van het bezwaar;", e);
			error(getString("error.onbekend"));
			return;
		}
	}

	private boolean isErEenBezwaarMet(BezwaarType type)
	{
		List<BezwaarGroupViewWrapper> bezwaarWrappers = bezwaarViewModels;
		for (BezwaarGroupViewWrapper groupWrapper : bezwaarWrappers)
		{
			for (BezwaarViewWrapper wrapper : groupWrapper.getBezwaren())
			{
				if (type.equals(wrapper.getType()) && Boolean.TRUE.equals(wrapper.getActief()))
				{
					return true;
				}
			}
		}
		return false;
	}

	private boolean heeftRechtVoor(Recht recht)
	{
		InstellingGebruiker ingelogdeGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		if (autorisatieService.getActieVoorMedewerker(ingelogdeGebruiker, null, recht) == null)
		{
			return false;
		}
		return true;
	}

	@Override
	protected void onDetach()
	{
		ModelUtil.nullSafeDetach(bezwaarModel);
		ModelUtil.nullSafeDetach(files);
		if (document != null)
		{
			ModelUtil.nullSafeDetach(document);
		}
		super.onDetach();
	}

	public ClientInzienBezwaarPanel getClientInzienBezwaarPanel()
	{
		return clientInzienBezwaarPanel;
	}

	public void setClientInzienPanel(ClientInzienBezwaarPanel clientInzienPanel)
	{
		this.clientInzienBezwaarPanel = clientInzienPanel;
	}

	public abstract void close(AjaxRequestTarget target);

}
