package nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange.followup;

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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.mamma.MammaFollowUpService;
import nl.rivm.screenit.main.service.mamma.MammaUitwisselportaalService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.form.BigDecimalField;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ImageIconCellPanel;
import nl.rivm.screenit.main.web.component.table.NotClickablePropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientTooltipPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange.MammaExchangeBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientTooltipType;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.MammaFollowUpDoorverwezenFilterOptie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpTumorGrootteClassificatie;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.radiochoice.BooleanRadioChoice;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.ajax.form.OnChangeAjaxBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxCheckBox;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.head.CssHeaderItem;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.html.TransparentWebMarkupContainer;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@SecurityConstraint(
	constraint = ShiroConstraint.HasPermission,
	checkScope = true,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
	recht = { Recht.GEBRUIKER_MAMMA_FOLLOW_UP_RADIOLOGIE },
	organisatieTypeScopes = { OrganisatieType.RADIOLOGIEAFDELING, OrganisatieType.MAMMAPOLI, OrganisatieType.ZORGINSTELLING })
public class MammaFollowUpRadiologieVerslagPage extends MammaExchangeBasePage
{
	private WebMarkupContainer passport = null;

	private WebMarkupContainer contentContainer = null;

	private WebMarkupContainer openstaandeRadiologieVerslagenContainer = null;

	@SpringBean
	private MammaFollowUpService followUpService;

	@SpringBean
	private MammaBaseScreeningrondeService screeningrondeService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private MammaUitwisselportaalService uitwisselportaalService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	private final TransparentWebMarkupContainer fragments;

	private final IModel<Instelling> instellingModel;

	private IModel<MammaFollowUpDoorverwezenFilterOptie> doorverwezenFilterOptieModel;

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(CssHeaderItem.forUrl("assets/font-awesome/css/font-awesome.min.css"));
	}

	public MammaFollowUpRadiologieVerslagPage()
	{
		if (ScreenitSession.get().isZoekObjectGezetForComponent(MammaFollowUpRadiologieVerslagPage.class))
		{
			doorverwezenFilterOptieModel = (IModel<MammaFollowUpDoorverwezenFilterOptie>) ScreenitSession.get().getZoekObject(MammaFollowUpRadiologieVerslagPage.class);
		}
		if (doorverwezenFilterOptieModel == null || doorverwezenFilterOptieModel.getObject() == null)
		{
			doorverwezenFilterOptieModel = Model.of(MammaFollowUpDoorverwezenFilterOptie.ALLES);
		}

		instellingModel = ModelUtil.sModel(ScreenitSession.get().getLoggedInInstellingGebruiker().getOrganisatie());
		createEmptyPassportContainer();
		createEmptyContentContainer();
		fragments = new TransparentWebMarkupContainer("fragments");
		createOpenstaandeBeelvormendVerslagenContainer();
		add(fragments);
	}

	@Override
	protected void updateContent()
	{
		AjaxRequestTarget target = RequestCycle.get().find(AjaxRequestTarget.class).orElse(null);
		if (clientOpt != null)
		{
			passport = new ClientPaspoortPanel("paspoort", clientOpt);
			passport.setOutputMarkupId(true);
			passport.setOutputMarkupPlaceholderTag(true);
			passport.setVisible(true);
			openstaandeRadiologieVerslagenContainer.setVisible(false);
			addOrReplace(passport);
			target.add(passport, openstaandeRadiologieVerslagenContainer);
			createContentContainer(clientOpt, target);
		}
		else
		{
			passport.setVisible(false);
			contentContainer.setVisible(false);

			openstaandeRadiologieVerslagenContainer.setVisible(true);
			target.add(passport, contentContainer, openstaandeRadiologieVerslagenContainer);
		}
	}

	private void createContentContainer(IModel<Client> clientOpt, AjaxRequestTarget target)
	{
		MammaScreeningRonde screeningRonde = screeningrondeService.getLaatsteScreeningRondeMetUitslag(clientOpt.getObject());

		WebMarkupContainer nieuwePanel;
		if (screeningRonde == null)
		{
			nieuwePanel = new GeenScreeningRondePanel("content");
		}
		else if (uitwisselportaalService.getFollowUpRadiologieVerslag(screeningRonde, ScreenitSession.get().getLoggedInInstellingGebruiker()) == null)
		{
			nieuwePanel = new GeenBeeldenGedownloadedPanel("content", ModelUtil.sModel(screeningRonde));
		}
		else
		{
			nieuwePanel = new FormulierPanel("content",
				ModelUtil.ccModel(uitwisselportaalService.getFollowUpRadiologieVerslag(screeningRonde, ScreenitSession.get().getLoggedInInstellingGebruiker())));
		}
		nieuwePanel.setOutputMarkupId(true);
		contentContainer.replaceWith(nieuwePanel);
		contentContainer = nieuwePanel;
		contentContainer.setVisible(true);
		target.add(contentContainer);
	}

	private void createEmptyContentContainer()
	{
		contentContainer = new EmptyPanel("content");
		contentContainer.setOutputMarkupId(true);
		contentContainer.setVisible(false);
		contentContainer.setOutputMarkupPlaceholderTag(true);
		add(contentContainer);
	}

	private void createEmptyPassportContainer()
	{
		passport = new EmptyPanel("paspoort");
		passport.setOutputMarkupId(true);
		passport.setVisible(false);
		passport.setOutputMarkupPlaceholderTag(true);
		addOrReplace(passport);
	}

	private void createOpenstaandeBeelvormendVerslagenContainer()
	{
		openstaandeRadiologieVerslagenContainer = new WebMarkupContainer("openstaandeRadiologieVerslagenContainer");
		openstaandeRadiologieVerslagenContainer.setOutputMarkupId(true);
		openstaandeRadiologieVerslagenContainer.setOutputMarkupPlaceholderTag(true);
		add(openstaandeRadiologieVerslagenContainer);

		createDoorverwezenFilter();

		MammaFollowUpRadiologieInstellingProvider followUpDataRegioProvider = new MammaFollowUpRadiologieInstellingProvider(instellingModel, doorverwezenFilterOptieModel);

		List<IColumn<MammaFollowUpRadiologieVerslag, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Bsn"), "screeningRonde.dossier.client.persoon.bsn"));
		columns.add(new GeboortedatumColumn<>("screeningRonde.dossier.client.persoon"));
		columns.add(new PropertyColumn<>(Model.of("Beelden gedownload op"), "aangemaaktOp", "aangemaaktOp"));
		columns.add(new NotClickablePropertyColumn<>(Model.of("Urgentie"), "")
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaFollowUpRadiologieVerslag>> item, String componentId, IModel<MammaFollowUpRadiologieVerslag> rowModel)
			{
				Date urgentVanaf = DateUtil
					.toUtilDate(dateSupplier.getLocalDate().minusDays(preferenceService.getInteger(PreferenceKey.MAMMA_FOLLOW_UP_RADIOLOGIE_WERKLIJST_NA_DOWNLOADEN.name())));
				if (rowModel.getObject().getAangemaaktOp().compareTo(urgentVanaf) <= 0)
				{
					item.add(new ImageIconCellPanel<>(componentId, rowModel, "icon-large-font fa fa-exclamation", "Urgent"));
				}
				else
				{
					item.add(new EmptyPanel(componentId));
				}
			}
		});

		ScreenitDataTable<MammaFollowUpRadiologieVerslag, String> openstaandeRadiologieVerslagenTabel = new ScreenitDataTable<>(
			"openstaandeRadiologieVerslagenTabel", columns,
			followUpDataRegioProvider, 10, Model.of("radiologieverslag(en)"))
		{

			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaFollowUpRadiologieVerslag> model)
			{
				clientOpt = ModelUtil.csModel(model.getObject().getScreeningRonde().getDossier().getClient());
				updateContent();
			}
		};
		openstaandeRadiologieVerslagenContainer.add(openstaandeRadiologieVerslagenTabel);
	}

	private void createDoorverwezenFilter()
	{
		RadioChoice<MammaFollowUpDoorverwezenFilterOptie> doorverwezenFilter = new RadioChoice<>("doorverwezenFilter", doorverwezenFilterOptieModel,
			Arrays.asList(MammaFollowUpDoorverwezenFilterOptie.values()),
			new EnumChoiceRenderer<>(this));
		doorverwezenFilter.setPrefix("<div class=\"span2\">\n" +
			"<div class=\"control-group\"><label class=\"radio\">");
		doorverwezenFilter.setSuffix("</label></div>\n" +
			"</div>");

		doorverwezenFilter.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
			{
				ScreenitSession.get().setZoekObject(MammaFollowUpRadiologieVerslagPage.class, doorverwezenFilterOptieModel);
				ajaxRequestTarget.add(openstaandeRadiologieVerslagenContainer);
			}
		});
		openstaandeRadiologieVerslagenContainer.add(doorverwezenFilter);
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(instellingModel);
		ModelUtil.nullSafeDetach(doorverwezenFilterOptieModel);
	}

	private class GeenScreeningRondePanel extends Fragment
	{
		public GeenScreeningRondePanel(String id)
		{
			super(id, "geenScreeningRondeFragment", fragments);
		}
	}

	private class GeenBeeldenGedownloadedPanel extends Fragment
	{
		public GeenBeeldenGedownloadedPanel(String id, IModel<MammaScreeningRonde> model)
		{
			super(id, "geenBeeldenGedownloadedFragment", fragments, model);
			add(new IndicatingAjaxLink<Void>("tochInvoeren")
			{
				@Override
				public void onClick(AjaxRequestTarget target)
				{
					MammaScreeningRonde screeningRonde = (MammaScreeningRonde) GeenBeeldenGedownloadedPanel.this.getDefaultModelObject();
					IModel<MammaFollowUpRadiologieVerslag> model;
					MammaFollowUpRadiologieVerslag followUpRadiologieVerslag = uitwisselportaalService.getFollowUpRadiologieVerslag(screeningRonde,
						ScreenitSession.get().getLoggedInInstellingGebruiker());
					if (followUpRadiologieVerslag == null)
					{
						MammaFollowUpRadiologieVerslag radiologieVerslag = new MammaFollowUpRadiologieVerslag();
						model = ModelUtil.ccModel(radiologieVerslag);
						radiologieVerslag = model.getObject();
						radiologieVerslag.setAangemaaktIn(ScreenitSession.get().getInstelling());
						radiologieVerslag.setAangemaaktOp(dateSupplier.getDate());
						radiologieVerslag.setInformatieBeschikbaar(true);
						radiologieVerslag.setScreeningRonde(screeningRonde);
					}
					else
					{
						model = ModelUtil.ccModel(followUpRadiologieVerslag);
					}

					WebMarkupContainer nieuwePanel = new FormulierPanel("content", model);
					nieuwePanel.setOutputMarkupId(true);
					contentContainer.replaceWith(nieuwePanel);
					contentContainer = nieuwePanel;
					contentContainer.setVisible(true);
					target.add(contentContainer);
				}
			});
		}
	}

	private class FormulierPanel extends Fragment
	{
		public FormulierPanel(String id, IModel<MammaFollowUpRadiologieVerslag> verslagModel)
		{
			super(id, "formulier", fragments, verslagModel);
			MammaFollowUpRadiologieVerslag followUpRadiologieVerslag = verslagModel.getObject();

			final Form<MammaFollowUpRadiologieVerslag> form = new Form<>("form", verslagModel);
			add(form);

			WebMarkupContainer valuesContainer = new WebMarkupContainer("valuesContainer");
			valuesContainer.setVisible(Boolean.TRUE.equals(followUpRadiologieVerslag.getInformatieBeschikbaar()));
			valuesContainer.setOutputMarkupId(true);
			valuesContainer.setOutputMarkupPlaceholderTag(true);
			form.add(valuesContainer);

			BigDecimalField tumorGrootte = (BigDecimalField) new BigDecimalField("radioloogTumorGrootte", 2, BigDecimal.ZERO,
				Constants.BK_MAXIMALE_TUMOR_GROOTTE).setRequired(false);
			tumorGrootte.add(new OnChangeAjaxBehavior()
			{
				@Override
				protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
				{
					BigDecimal tumorGrootteValue = tumorGrootte.getConvertedInput();
					MammaFollowUpTumorGrootteClassificatie classificatie = tumorGrootteValue != null
						? MammaFollowUpTumorGrootteClassificatie.getClassificatie(tumorGrootteValue)
						: null;
					verslagModel.getObject().setRadioloogTumorGrootteClassificatie(classificatie);
					addOrReplaceClassificatieLabel(valuesContainer, ajaxRequestTarget);
				}
			});

			valuesContainer.add(tumorGrootte);
			valuesContainer.add(new ClientTooltipPanel("tumorGrootteTooltip", ClientTooltipType.MAMMA_TUMORGROOTTE, false));

			RadioChoice<Boolean> pathologieUitgevoerd = new BooleanRadioChoice("pathologieUitgevoerd");
			pathologieUitgevoerd.setPrefix("<label class=\"radio\">");
			pathologieUitgevoerd.setSuffix("</label>");
			pathologieUitgevoerd.setRequired(true);
			pathologieUitgevoerd.add(new AjaxFormChoiceComponentUpdatingBehavior()
			{
				@Override
				protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
				{
					addOrReplaceTumorVelden(ajaxRequestTarget, tumorGrootte, valuesContainer);
					ajaxRequestTarget.add(valuesContainer);
				}
			});
			valuesContainer.add(pathologieUitgevoerd);

			addOrReplaceTumorVelden(null, tumorGrootte, valuesContainer);

			valuesContainer.add(
				new ScreenitDropdown<>("conclusieBirads", Arrays.asList(MammaFollowUpBIRADSWaarde.values()), new EnumChoiceRenderer<>()).setNullValid(false).setRequired(true));

			TextArea<String> conclusieEersteUitslagRadiologie = new TextArea<>("conclusieEersteUitslagRadiologie");
			conclusieEersteUitslagRadiologie.add(StringValidator.maximumLength(HibernateMagicNumber.L1024));
			conclusieEersteUitslagRadiologie.setRequired(true);
			valuesContainer.add(conclusieEersteUitslagRadiologie);

			AjaxCheckBox informatieBeschikbaar = new AjaxCheckBox("informatieBeschikbaar")
			{
				@Override
				protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
				{
					MammaFollowUpRadiologieVerslag followUpRadiologieVerslag = verslagModel.getObject();
					valuesContainer.setVisible(Boolean.TRUE.equals(followUpRadiologieVerslag.getInformatieBeschikbaar()));
					ajaxRequestTarget.add(valuesContainer);
				}
			};

			form.add(informatieBeschikbaar);

			form.add(new IndicatingAjaxSubmitLink("opslaan")
			{
				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					super.onSubmit(target);
					MammaFollowUpRadiologieVerslag verslag = form.getModelObject();
					if (Boolean.FALSE.equals(verslag.getInformatieBeschikbaar()))
					{
						verslag.setPathologieUitgevoerd(null);
						verslag.setRadioloogTumorGrootte(null);
						verslag.setRadioloogTumorGrootteClassificatie(null);
						verslag.setConclusieEersteUitslagRadiologie(null);
						verslag.setConclusieBirads(null);
					}
					else if (Boolean.FALSE.equals(verslag.getPathologieUitgevoerd()))
					{
						verslag.setRadioloogTumorGrootte(null);
						verslag.setRadioloogTumorGrootteClassificatie(null);
					}
					followUpService.saveOrUpdateRadiologie(verslag, ScreenitSession.get().getLoggedInInstellingGebruiker());
					ScreenitSession.get().success(getString("message.gegevensopgeslagen"));
					setResponsePage(MammaFollowUpRadiologieVerslagPage.class);
				}
			});
		}

		private void addOrReplaceTumorVelden(AjaxRequestTarget ajaxRequestTarget, BigDecimalField tumorGrootte, WebMarkupContainer valuesContainer)
		{
			MammaFollowUpRadiologieVerslag verslag = (MammaFollowUpRadiologieVerslag) FormulierPanel.this.getDefaultModelObject();
			tumorGrootte.setVisible(Boolean.TRUE.equals(verslag.getPathologieUitgevoerd()));
			addOrReplaceClassificatieLabel(valuesContainer, ajaxRequestTarget);
		}

		private void addOrReplaceClassificatieLabel(WebMarkupContainer valueForm, AjaxRequestTarget target)
		{
			EnumLabel<MammaFollowUpTumorGrootteClassificatie> classificatieLabel = new EnumLabel<>("radioloogTumorGrootteClassificatie");
			MammaFollowUpRadiologieVerslag verslag = (MammaFollowUpRadiologieVerslag) FormulierPanel.this.getDefaultModelObject();

			classificatieLabel.setVisible(verslag.getRadioloogTumorGrootteClassificatie() != null && Boolean.TRUE.equals(verslag.getInformatieBeschikbaar())
				&& Boolean.TRUE.equals(verslag.getPathologieUitgevoerd()));
			classificatieLabel.setOutputMarkupId(true);
			classificatieLabel.setOutputMarkupPlaceholderTag(true);
			valueForm.addOrReplace(classificatieLabel);

			if (target != null)
			{
				target.add(classificatieLabel);
			}
		}
	}
}
