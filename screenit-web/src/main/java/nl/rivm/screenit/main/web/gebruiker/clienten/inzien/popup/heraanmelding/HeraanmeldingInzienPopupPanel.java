package nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.heraanmelding;

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
import java.util.Collections;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.comparator.BriefCreatieDatumComparator;
import nl.rivm.screenit.main.model.GebeurtenisBron;
import nl.rivm.screenit.main.service.BriefService;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.util.BriefOmschrijvingUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.DocumentVervangenPanel;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.BriefUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.io.FilenameUtils;
import org.apache.shiro.util.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.link.DownloadLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class HeraanmeldingInzienPopupPanel<A extends Afmelding<?, ?, ?>, B extends ClientBrief<?, ?, ?>> extends GenericPanel<A>
{
	private static final long serialVersionUID = 1L;

	private IModel<UploadDocument> uploadDocumentModel;

	@SpringBean
	private FileService fileService;

	@SpringBean
	private DossierService dossierService;

	@SpringBean
	private BriefService briefService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private ClientService clientService;

	private Actie actie;

	private WebMarkupContainer uploadForm;

	private IModel<List<FileUpload>> files;

	protected HeraanmeldingInzienPopupPanel(String id, IModel<A> model)
	{
		super(id, model);
		files = new ListModel<>();

		actie = autorisatieService.getActieVoorMedewerker(ScreenitSession.get().getLoggedInInstellingGebruiker(), ScreenitSession.get().getCurrentSelectedMedewerker(),
			Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN);

		add(new Label("wijzeHeraanmelding", getWijzeVanHeraanmeldenTekst(model.getObject())));

		WebMarkupContainer verstuurdFormulierContainer = new WebMarkupContainer("formulierVerstuurdContainer");
		add(verstuurdFormulierContainer);
		verstuurdFormulierContainer.add(new ListView<String>("brievenLijst", creatieDatumCreaterHeraanmelding(model.getObject()))
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<String> item)
			{
				String tekst = item.getModelObject();
				item.add(new Label("brief", Model.<String> of(tekst)));
			}

		});

		add(DateLabel.forDatePattern("heraanmeldDatum", new PropertyModel<Date>(model, "heraanmeldDatum"), "dd-MM-yyyy"));

		uploadDocumentModel = ModelUtil.sModel(getModelObject().getHandtekeningDocumentHeraanmelding());
		if (uploadDocumentModel != null && uploadDocumentModel.getObject() != null)
		{
			add(new DownloadLink("heraanmeldformulierHandImg", new LoadableDetachableModel<File>()
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected File load()
				{
					return fileService.load(uploadDocumentModel.getObject());
				}

			}, "Heraanmeldformulier met handtekening." + FilenameUtils.getExtension(uploadDocumentModel.getObject().getNaam())));
		}
		else
		{
			EmptyPanel empty = new EmptyPanel("heraanmeldformulierHandImg");
			empty.setVisible(false);
			add(empty);
		}

		addVervangenPanel(model);
		addButtons();
	}

	private void addButtons()
	{
		add(new AjaxLink<Void>("nogmaalsVersturen")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ClientBrief brief = getLaatsteBrief();
				briefService.opnieuwAanmaken(brief, ScreenitSession.get().getLoggedInAccount());

				info(getString("info.heraanmeldingnogmaalsverstuurd"));
				close(target);
			}

		}.setVisible(DossierStatus.ACTIEF == getModelObject().getDossier().getStatus()));
		add(new AjaxLink<Void>("tegenhouden")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ClientBrief brief = getLaatsteBrief();
				brief.setTegenhouden(true);
				hibernateService.saveOrUpdate(brief);

				logService.logGebeurtenis(LogGebeurtenis.BRIEF_TEGENHOUDEN, ScreenitSession.get().getLoggedInAccount(), brief.getClient(),
					BriefUtil.getBriefTypeNaam(brief) + ", wordt tegengehouden.", brief.getBriefType().getOnderzoeken());
				info(getString("info.brieftegenhouden"));
				close(target);
			}
		}.setVisible(Actie.AANPASSEN.equals(actie) && getLaatsteBrief() != null && !getLaatsteBrief().isTegenhouden() && getLaatsteBrief().getMergedBrieven() == null));
		add(new AjaxLink<Void>("doorvoeren")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ClientBrief brief = getLaatsteBrief();
				brief.setTegenhouden(false);
				hibernateService.saveOrUpdate(brief);
				logService.logGebeurtenis(LogGebeurtenis.BRIEF_DOORVOEREN, ScreenitSession.get().getLoggedInAccount(), brief.getClient(),
					brief.getBriefType() + ", was tegengehouden en wordt nu doorgevoerd.", brief.getBriefType().getOnderzoeken());
				info(getString("info.briefactiveren"));
				close(target);
			}
		}.setVisible(Actie.AANPASSEN == actie && getLaatsteBrief() != null && getLaatsteBrief().isTegenhouden()));
		add(new AjaxLink<Void>("vervangen")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				uploadForm.setVisible(true);
				target.add(uploadForm);
			}
		}.setVisible(ScreenitSession.get().checkPermission(Recht.VERVANGEN_DOCUMENTEN, Actie.AANPASSEN) && getModelObject().getHandtekeningDocumentHeraanmelding() != null));
	}

	private void addVervangenPanel(IModel<A> model)
	{
		uploadForm = new DocumentVervangenPanel("documentVervangen")
		{
			@Override
			protected void vervangDocument(UploadDocument uploadDocument, AjaxRequestTarget target)
			{
				if (clientService.vervangHeraanmeldingDocument(uploadDocument, model.getObject(), uploadDocumentModel.getObject(), getLaatsteBrief(),
					ScreenitSession.get().getLoggedInAccount()))
				{
					info(getString("info.vervangendocument"));
					close(target);
				}
				else
				{
					error(getString("error.onbekend"));
				}
			}
		};
		uploadForm.setVisible(false);
		uploadForm.setOutputMarkupId(true);
		uploadForm.setOutputMarkupPlaceholderTag(true);
		add(uploadForm);
	}

	private ClientBrief getLaatsteBrief()
	{
		List<? extends ClientBrief> brieven = briefService.getBrievenVanAfmelding(getModelObject(), true);
		Collections.sort(brieven, new BriefCreatieDatumComparator().reversed());
		if (!CollectionUtils.isEmpty(brieven))
		{
			return brieven.get(0);
		}
		return null;
	}

	private List<String> creatieDatumCreaterHeraanmelding(A afmelding)
	{
		List<? extends ClientBrief> brieven = briefService.getBrievenVanAfmelding(afmelding, true);
		return BriefOmschrijvingUtil.getBrievenOmschrijvingen(brieven, this::getString);
	}

	private String getWijzeVanHeraanmeldenTekst(A afmelding)
	{
		String wijzeHeraanmelding = null;

		GebeurtenisBron bron = dossierService.bepaalGebeurtenisBron(afmelding);

		switch (afmelding.getBevolkingsonderzoek())
		{
		case COLON:
			if (bron != null)
			{
				switch (bron)
				{
				case MEDEWERKER:
					if (ColonAfmeldingReden.ONTERECHT.equals(((ColonAfmelding) HibernateHelper.deproxy(afmelding)).getReden()))
					{
						wijzeHeraanmelding = "correctieantwoordformulier";
					}
					else
					{
						wijzeHeraanmelding = "infolijn";
					}
					break;
				case AUTOMATISCH:
					if (ColonAfmeldingReden.ONTERECHT.equals(((ColonAfmelding) HibernateHelper.deproxy(afmelding)).getReden()))
					{
						wijzeHeraanmelding = "correctieantwoordformulier";
					}
					else
					{
						wijzeHeraanmelding = "antwoordformulier";
					}
					break;
				case CLIENT:
					wijzeHeraanmelding = "clientportaal";
					break;
				default:
					wijzeHeraanmelding = "";
				}
			}
			else if (bron == null && ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK.equals(((ColonAfmelding) HibernateHelper.deproxy(afmelding)).getReden()))
			{
				wijzeHeraanmelding = "proefbevolkingsonderzoek";
			}
			break;
		case CERVIX:
			switch (bron)
			{
			case MEDEWERKER:
				wijzeHeraanmelding = "infolijn";
				break;
			case CLIENT:
				wijzeHeraanmelding = "clientportaal";
				break;
			case AUTOMATISCH:
				wijzeHeraanmelding = "monster";
				break;
			default:
				throw new IllegalStateException();
			}
			break;
		case MAMMA:
			wijzeHeraanmelding = getMammaWijzeVanHeraanmeldenTekst(bron);
			break;
		default:
			throw new IllegalStateException();
		}

		if (wijzeHeraanmelding == null)
		{
			return "";
		}

		return getString("label.wijzevanafmelding." + wijzeHeraanmelding);
	}

	private String getMammaWijzeVanHeraanmeldenTekst(GebeurtenisBron bron)
	{
		if (bron == null || bron == GebeurtenisBron.MEDEWERKER) 
		{
			return "infolijn";
		}
		else if (bron == GebeurtenisBron.CLIENT)
		{
			return "clientportaal";
		}
		throw new IllegalStateException();
	}

	protected abstract void close(AjaxRequestTarget target);

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(uploadDocumentModel);
		ModelUtil.nullSafeDetach(files);
	}
}
