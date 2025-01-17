package nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.heraanmelding;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;

import nl.rivm.screenit.comparator.BriefCreatieDatumComparator;
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
import nl.rivm.screenit.model.enums.GebeurtenisBron;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.BriefUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.io.FilenameUtils;
import org.apache.shiro.util.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
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
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public abstract class HeraanmeldingInzienPopupPanel<A extends Afmelding<?, ?, ?>, B extends ClientBrief<?, ?, ?>> extends GenericPanel<A>
{
	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	private DossierService dossierService;

	@SpringBean
	private BriefService briefService;

	@SpringBean
	private BaseBriefService baseBriefService;

	@SpringBean
	private BriefHerdrukkenService briefHerdrukkenService;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private BaseAfmeldService baseAfmeldService;

	private IModel<UploadDocument> uploadDocumentModel;

	private final Actie actie;

	private WebMarkupContainer uploadForm;

	private final IModel<List<FileUpload>> files = new ListModel<>();

	protected HeraanmeldingInzienPopupPanel(String id, IModel<A> model)
	{
		super(id, model);
		actie = autorisatieService.getActieVoorMedewerker(ScreenitSession.get().getLoggedInInstellingGebruiker(), ScreenitSession.get().getCurrentSelectedMedewerker(),
			Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		add(new Label("wijzeHeraanmelding", getWijzeVanHeraanmeldenTekst()));

		WebMarkupContainer verstuurdFormulierContainer = new WebMarkupContainer("formulierVerstuurdContainer");
		add(verstuurdFormulierContainer);
		verstuurdFormulierContainer.add(new ListView<>("brievenLijst", creatieDatumCreaterHeraanmelding())
		{
			@Override
			protected void populateItem(ListItem<String> item)
			{
				String tekst = item.getModelObject();
				item.add(new Label("brief", Model.of(tekst)));
			}

		});

		add(DateLabel.forDatePattern("heraanmeldDatum", new PropertyModel<>(getModel(), "heraanmeldDatum"), "dd-MM-yyyy"));

		uploadDocumentModel = ModelUtil.sModel(getModelObject().getHandtekeningDocumentHeraanmelding());
		if (uploadDocumentModel != null && uploadDocumentModel.getObject() != null)
		{
			add(new DownloadLink("heraanmeldformulierHandImg", new LoadableDetachableModel<>()
			{
				@Override
				protected File load()
				{
					return uploadDocumentService.load(uploadDocumentModel.getObject());
				}

			}, "Heraanmeldformulier met handtekening." + FilenameUtils.getExtension(uploadDocumentModel.getObject().getNaam())));
		}
		else
		{
			EmptyPanel empty = new EmptyPanel("heraanmeldformulierHandImg");
			empty.setVisible(false);
			add(empty);
		}

		addVervangenPanel();
		addButtons();
	}

	private void addButtons()
	{
		ClientBrief laatsteBrief = getLaatsteBrief();

		add(new AjaxLink<Void>("nogmaalsVersturen")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ClientBrief brief = getLaatsteBrief();
				briefHerdrukkenService.opnieuwAanmaken(brief, ScreenitSession.get().getLoggedInAccount());

				info(getString("info.heraanmeldingnogmaalsverstuurd"));
				close(target);
			}

		}.setVisible(DossierStatus.ACTIEF == (getModelObject().getDossier() == null ?
			getModelObject().getScreeningRonde().getDossier().getStatus() :
			getModelObject().getDossier().getStatus())));
		add(new AjaxLink<Void>("tegenhouden")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				baseBriefService.briefTegenhouden(getLaatsteBrief(), ScreenitSession.get().getLoggedInAccount());
				info(getString("info.brieftegenhouden"));
				close(target);
			}
		}.setVisible(Actie.AANPASSEN.equals(actie) && laatsteBrief != null && !BriefUtil.isTegengehouden(laatsteBrief) && BriefUtil.getMergedBrieven(laatsteBrief) == null));
		add(new AjaxLink<Void>("doorvoeren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				baseBriefService.briefNietMeerTegenhouden(getLaatsteBrief(), ScreenitSession.get().getLoggedInAccount());
				info(getString("info.briefactiveren"));
				close(target);
			}
		}.setVisible(Actie.AANPASSEN == actie && BriefUtil.isTegengehouden(laatsteBrief)));
		add(new AjaxLink<Void>("vervangen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				uploadForm.setVisible(true);
				target.add(uploadForm);
			}
		}.setVisible(ScreenitSession.get().checkPermission(Recht.VERVANGEN_DOCUMENTEN, Actie.AANPASSEN) && getModelObject().getHandtekeningDocumentHeraanmelding() != null));
	}

	private void addVervangenPanel()
	{
		uploadForm = new DocumentVervangenPanel("documentVervangen")
		{
			@Override
			protected void vervangDocument(UploadDocument uploadDocument, AjaxRequestTarget target)
			{
				if (baseAfmeldService.vervangHeraanmeldingDocument(uploadDocument, getModelObject(), uploadDocumentModel.getObject(), getLaatsteBrief(),
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

	private List<String> creatieDatumCreaterHeraanmelding()
	{
		A afmelding = getModelObject();
		List<? extends ClientBrief> brieven = briefService.getBrievenVanAfmelding(afmelding, true);
		return BriefOmschrijvingUtil.getBrievenOmschrijvingen(brieven);
	}

	private String getWijzeVanHeraanmeldenTekst()
	{
		String wijzeHeraanmelding = null;
		A afmelding = getModelObject();
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
			else if (ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK.equals(((ColonAfmelding) HibernateHelper.deproxy(afmelding)).getReden()))
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
