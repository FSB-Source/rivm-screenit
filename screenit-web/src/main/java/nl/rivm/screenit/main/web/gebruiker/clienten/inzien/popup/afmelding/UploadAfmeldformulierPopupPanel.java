package nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.afmelding;

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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.comparator.BriefCreatieDatumComparator;
import nl.rivm.screenit.main.service.BriefService;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.util.BriefOmschrijvingUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.enums.CervixAfmeldingReden;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.GebeurtenisBron;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.enums.MammaAfmeldingReden;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.BriefUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

import org.apache.shiro.util.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.RadioChoice;
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

@Slf4j
public abstract class UploadAfmeldformulierPopupPanel<A extends Afmelding> extends GenericPanel<A>
{
	@SpringBean
	private BaseAfmeldService baseAfmeldService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private DossierService dossierService;

	@SpringBean
	private BriefService briefService;

	@SpringBean
	private BaseBriefService baseBriefService;

	@SpringBean
	private BriefHerdrukkenService briefHerdrukkenService;

	public UploadAfmeldformulierPopupPanel(final String id, IModel<A> model)
	{
		super(id, new CompoundPropertyModel<>(model));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		boolean magTegenhouden = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN, Actie.AANPASSEN);

		Dossier dossier = getModelObject().getDossier();

		IModel<List<FileUpload>> files = new ListModel<>();

		Form<Void> uploadForm = new Form<>("uploadForm");

		uploadForm.add(new Label("title", getString(getTitleResourceString())));

		WebMarkupContainer formulierAanwezig = new WebMarkupContainer("formulierAanwezigContainer");
		uploadForm.add(formulierAanwezig);

		FileUploadField upload = new FileUploadField("fileUpload", files);
		upload.add(new FileValidator(FileType.PDF));
		upload.setRequired(true);
		upload.setEnabled(DossierStatus.ACTIEF == dossier.getStatus());
		formulierAanwezig.add(upload);

		uploadForm.add(new AjaxSubmitLink("doorgaan")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (files.getObject().size() == 1)
				{

					FileUpload fileUpload = files.getObject().get(0);
					try
					{
						A afmelding = UploadAfmeldformulierPopupPanel.this.getModelObject();

						File definitieFile = fileUpload.writeToTempFile();
						UploadDocument document = new UploadDocument();
						document.setActief(Boolean.TRUE);
						document.setContentType(fileUpload.getContentType());
						document.setFile(definitieFile);
						document.setNaam(fileUpload.getClientFileName());
						afmelding.setHandtekeningDocumentAfmelding(document);
						baseAfmeldService.afmelden(afmelding.getDossier().getClient(), afmelding, ScreenitSession.get().getLoggedInInstellingGebruiker());
						logService.logGebeurtenis(LogGebeurtenis.AFMELDEN, ScreenitSession.get().getLoggedInAccount(), afmelding.getDossier().getClient(),
							"Type: " + afmelding.getType().name().toLowerCase(), afmelding.getBevolkingsonderzoek());
						close(target);

					}
					catch (Exception e)
					{
						LOG.error("Fout bij uploaden van een afmeldingformulier met handtekening: ", e);
						error(getString("error.onbekend"));
					}
				}
				else
				{
					LOG.error("Er mag maar 1 bestand geuploaded worden als afmeldingformulier met handtekening");
					error(getString("error.onjuistaantalfiles"));
				}
			}

		}.setVisible(DossierStatus.ACTIEF == dossier.getStatus()));
		uploadForm.add(new Label("wijzeAfmelding", Model.of(getWijzeVanAfmeldingTekst(getModelObject()))));
		uploadForm.add(new AjaxLink<Void>("nogmaalsVersturen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ClientBrief brief = getOrgineleLaatsteBrief();
				briefHerdrukkenService.opnieuwAanmaken(brief, ScreenitSession.get().getLoggedInAccount());

				info(getString("info.afmeldingnogmaalsverstuurd"));
				close(target);
			}

		}.setVisible(DossierStatus.ACTIEF == dossier.getStatus()));

		ClientBrief laatsteBrief = getLaatsteBrief();
		uploadForm.add(new AjaxLink<Void>("tegenhouden")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				baseBriefService.briefTegenhouden(getLaatsteBrief(), ScreenitSession.get().getLoggedInAccount());
				info(getString("info.brieftegenhouden"));
				close(target);
			}
		}.setVisible(magTegenhouden && laatsteBrief != null && !BriefUtil.isTegengehouden(laatsteBrief) && BriefUtil.getMergedBrieven(laatsteBrief) == null));
		uploadForm.add(new AjaxLink<Void>("doorvoeren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				baseBriefService.briefNietMeerTegenhouden(getLaatsteBrief(), ScreenitSession.get().getLoggedInAccount());
				info(getString("info.briefactiveren"));
				close(target);
			}
		}.setVisible(magTegenhouden && BriefUtil.isTegengehouden(laatsteBrief)));
		uploadForm.add(new ListView<>("brievenLijst", creatieDatumCreaterAfmelding(getModelObject()))
		{
			@Override
			protected void populateItem(ListItem<String> item)
			{
				String tekst = item.getModelObject();
				item.add(new Label("brief", Model.of(tekst)));
			}
		});
		uploadForm.add(new AjaxLink<Void>("geenHandtekening")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				A afmelding = UploadAfmeldformulierPopupPanel.this.getModelObject();

				baseAfmeldService.definitieveAfmeldingAanvragen(afmelding.getDossier().getClient(), afmelding, true, ScreenitSession.get().getLoggedInInstellingGebruiker());

				info(getString("info.afmeldinghandtekeningverstuurd"));
				close(target);
			}

			@Override
			public boolean isVisible()
			{
				Afmelding afmelding = UploadAfmeldformulierPopupPanel.this.getModelObject();
				return BriefType.COLON_AFMELDING_AANVRAAG.equals(afmelding.getAfmeldingAanvraag().getBriefType())
					|| BriefType.CERVIX_AFMELDING_AANVRAAG.equals(afmelding.getAfmeldingAanvraag().getBriefType())
					|| BriefType.MAMMA_AFMELDING_AANVRAAG.equals(afmelding.getAfmeldingAanvraag().getBriefType());
			}
		});

		switch (getModelObject().getBevolkingsonderzoek())
		{
		case COLON:
			List<ColonAfmeldingReden> colonAfmeldingRedenen = new ArrayList<>(Arrays.asList(ColonAfmeldingReden.values()));
			colonAfmeldingRedenen.remove(ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK);
			colonAfmeldingRedenen.remove(ColonAfmeldingReden.ONTERECHT);
			RadioChoice<ColonAfmeldingReden> colonAfmeldingReden = new RadioChoice<>("reden", colonAfmeldingRedenen, new EnumChoiceRenderer<>(this));
			colonAfmeldingReden.setPrefix("<label class=\"radio\">");
			colonAfmeldingReden.setSuffix("</label>");
			colonAfmeldingReden.setOutputMarkupId(true);
			colonAfmeldingReden.setEnabled(DossierStatus.ACTIEF == dossier.getStatus());
			formulierAanwezig.add(colonAfmeldingReden);
			break;
		case CERVIX:
			List<CervixAfmeldingReden> cervixAfmeldingRedenen = new ArrayList<>(Arrays.asList(CervixAfmeldingReden.values()));
			RadioChoice<CervixAfmeldingReden> cervixAfmeldingReden = new RadioChoice<>("reden", cervixAfmeldingRedenen, new EnumChoiceRenderer<>(this));
			cervixAfmeldingReden.setPrefix("<label class=\"radio\">");
			cervixAfmeldingReden.setSuffix("</label>");
			cervixAfmeldingReden.setOutputMarkupId(true);
			formulierAanwezig.add(cervixAfmeldingReden);
			break;
		case MAMMA:
			RadioChoice<MammaAfmeldingReden> mammaAfmeldingReden = new RadioChoice<>("reden", MammaAfmeldingReden.definitieveRedenen(), new EnumChoiceRenderer<>(this));
			mammaAfmeldingReden.setPrefix("<label class=\"radio\">");
			mammaAfmeldingReden.setSuffix("</label>");
			mammaAfmeldingReden.setOutputMarkupId(true);
			mammaAfmeldingReden.setRequired(true);
			formulierAanwezig.add(mammaAfmeldingReden);
			break;
		default:
			break;
		}
		add(uploadForm);
	}

	private String getTitleResourceString()
	{
		return "title.aanvraag.afmelden";
	}

	private String getWijzeVanAfmeldingTekst(A afmelding)
	{
		String wijzeAfmelding = "";
		GebeurtenisBron bron = dossierService.bepaalGebeurtenisBron(afmelding);
		if (bron != null)
		{
			switch (bron)
			{
			case MEDEWERKER:
				if (Bevolkingsonderzoek.COLON.equals(afmelding.getBevolkingsonderzoek())
					&& (ColonAfmeldingReden.ONTERECHT.equals(((ColonAfmelding) HibernateHelper.deproxy(afmelding)).getReden())
						|| ((ColonAfmelding) HibernateHelper.deproxy(afmelding)).getReden() == null))
				{
					wijzeAfmelding = "correctieantwoordformulier";
				}
				else
				{
					wijzeAfmelding = "infolijn";
				}
				break;
			case AUTOMATISCH:
				if (Bevolkingsonderzoek.COLON.equals(afmelding.getBevolkingsonderzoek())
					&& ColonAfmeldingReden.ONTERECHT.equals(((ColonAfmelding) HibernateHelper.deproxy(afmelding)).getReden()))
				{
					wijzeAfmelding = "correctieantwoordformulier";
				}
				else
				{
					wijzeAfmelding = "antwoordformulier";
				}
				break;
			case CLIENT:
				wijzeAfmelding = "clientportaal";
				break;
			default:
				wijzeAfmelding = "";
			}
		}

		if (wijzeAfmelding.equals(""))
		{
			return "";
		}

		return getString("label.wijzevanafmelding." + wijzeAfmelding);
	}

	private ClientBrief getOrgineleLaatsteBrief()
	{
		ClientBrief brief = null;
		A afmelding = UploadAfmeldformulierPopupPanel.this.getModelObject();
		if (afmelding.getAfmeldingAanvraag() != null)
		{
			brief = afmelding.getAfmeldingAanvraag();
		}

		return brief;
	}

	private ClientBrief getLaatsteBrief()
	{
		List<? extends ClientBrief> brieven = briefService.getBrievenVanAfmelding(getModelObject(), false);
		Collections.sort(brieven, new BriefCreatieDatumComparator().reversed());
		if (!CollectionUtils.isEmpty(brieven))
		{
			return brieven.get(0);
		}
		return null;
	}

	private List<String> creatieDatumCreaterAfmelding(A afmelding)
	{
		List<? extends ClientBrief> brieven = briefService.getBrievenVanAfmelding(afmelding, false);
		return BriefOmschrijvingUtil.getBrievenOmschrijvingen(brieven, this::getString);
	}

	public abstract void close(AjaxRequestTarget target);
}
