package nl.rivm.screenit.main.web.gebruiker.screening.colon.gebieden;

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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import nl.rivm.screenit.dao.colon.impl.ColonRestrictions;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.PercentageIntegerField;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.CapaciteitsPercWijziging;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.ColonUitnodigingsgebiedService;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.PercentageUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.model.MapModel;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxCallListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.head.CssHeaderItem;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.head.PriorityHeaderItem;
import org.apache.wicket.markup.html.TransparentWebMarkupContainer;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

import com.google.common.primitives.Ints;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	constraint = ShiroConstraint.HasPermission,
	checkScope = true,
	level = ToegangLevel.REGIO,
	recht = Recht.GEBRUIKER_BEHEER_GEBIEDEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class GebiedGegevens extends GebiedenBeheerPage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private LogService logService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ColonUitnodigingsgebiedService uitnodigingsGebiedService;

	@SpringBean
	private InstellingService instellingService;

	private Map<String, Integer> newAdherentiePercentages;

	private TransparentWebMarkupContainer fragments;

	private WebMarkupContainer controleResultaatPanel;

	private IModel<ColoscopieCentrum> intakelocatieModel = new SimpleHibernateModel<ColoscopieCentrum>();

	private IModel<List<ColoscopieCentrum>> intakelocatiesModel;

	private BootstrapDialog dialog;

	private Map<Long, IModel<ColoscopieCentrumColonCapaciteitVerdeling>> verwijderdeItemModels = new HashMap<>();

	private ScreenitDataTable<ColoscopieCentrumColonCapaciteitVerdeling, String> adherentieTabel;

	private Form<UitnodigingsGebied> adherentieForm;

	public GebiedGegevens(IModel<UitnodigingsGebied> model)
	{
		this(model, getGesplitsOpPostcode(model));
	}

	public GebiedGegevens(IModel<UitnodigingsGebied> model, Boolean gesplitsOpPostcode)
	{
		setDefaultModel(model);

		add(new GemeentePaspoortPanel("paspoort", new CompoundPropertyModel<Gemeente>(new PropertyModel<Gemeente>(model, "gemeente"))));

		Form<UitnodigingsGebied> form = new Form<>("form", model);
		add(form);
		form.add(new TextField<>("naam").setRequired(true).setEnabled(model.getObject().getPostcodeGebied() != null || StringUtils.isNotBlank(model.getObject().getWoonplaats())));
		boolean aanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_GEBIEDEN, Actie.AANPASSEN);

		PercentageIntegerField percentageIFobtRetour = new PercentageIntegerField("percentageIFobtRetour", 1);
		percentageIFobtRetour.setEnabled(aanpassen && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_GEBIEDEN_PERC_IFOBT_RETOUR, Actie.AANPASSEN));
		form.add(percentageIFobtRetour);

		PercentageIntegerField percentageOngunstigeIfobt = new PercentageIntegerField("percentageOngunstigeIfobt", 1);
		percentageOngunstigeIfobt.setEnabled(aanpassen && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_GEBIEDEN_PERC_ONGUNSTIGE_IFOBT, Actie.AANPASSEN));
		form.add(percentageOngunstigeIfobt);

		if (aanpassen)
		{
			if (Boolean.FALSE.equals(gesplitsOpPostcode))
			{
				form.add(new WoonplaatsKiezenPanel("postcodeWoonplaats", model));
			}
			else if (Boolean.TRUE.equals(gesplitsOpPostcode))
			{
				form.add(new PostcodeRangePanel("postcodeWoonplaats", model, form));
			}
			else
			{
				form.add(new WebMarkupContainer("postcodeWoonplaats").setVisible(false));
			}
		}
		else
		{
			form.add(new WebMarkupContainer("postcodeWoonplaats").setVisible(false));
		}

		adherentieBeheer(model);

		ScreenitIndicatingAjaxSubmitLink opslaan = new ScreenitIndicatingAjaxSubmitLink("opslaan", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				UitnodigingsGebied uitnodigingsGebied = (UitnodigingsGebied) form.getDefaultModelObject();
				if (uitnodigingsGebied.getId() == null)
				{
					uitnodigingsGebied.getGemeente().getUitnodigingsGebieden().add(uitnodigingsGebied);
				}
				hibernateService.saveOrUpdate(uitnodigingsGebied);
				BasePage.markeerFormulierenOpgeslagen(target);
				setResponsePage(new GemeenteGegevens(ModelUtil.cRModel(uitnodigingsGebied.getGemeente())));
			}
		};
		opslaan.setVisible(aanpassen);
		form.add(opslaan);

		form.add(new AjaxLink<UitnodigingsGebied>("annuleren")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				UitnodigingsGebied uitnodigingsGebied = getPageModel().getObject();
				setResponsePage(new GemeenteGegevens(ModelUtil.cRModel(uitnodigingsGebied.getGemeente())));
			}
		});

		fragments = new TransparentWebMarkupContainer("fragments");
		add(fragments);

		dialog = new BootstrapDialog("dialog");
		add(dialog);

	}

	private void adherentieBeheer(IModel<UitnodigingsGebied> model)
	{
		adherentieForm = new ScreenitForm<>("adherentieForm", model);
		adherentieForm.setOutputMarkupId(true);
		add(adherentieForm);

		List<ColoscopieCentrum> actieveIntakelocaties = instellingService.getActieveIntakelocaties();
		for (ColoscopieCentrumColonCapaciteitVerdeling verdeling : getPageModel().getObject().getVerdeling())
		{
			actieveIntakelocaties.remove(verdeling.getColoscopieCentrum());
		}
		intakelocatiesModel = ModelUtil.listRModel(actieveIntakelocaties);
		final ScreenitDropdown<ColoscopieCentrum> intakelocaties = ComponentHelper.newDropDownChoice("intakelocaties", intakelocatiesModel,
			new ChoiceRenderer<ColoscopieCentrum>("naam"));
		intakelocaties.setModel(new CompoundPropertyModel<>(new PropertyModel<ColoscopieCentrum>(GebiedGegevens.this, "intakelocatie")));
		adherentieForm.add(intakelocaties);

		initAdherentiePercentages();

		List<IColumn<ColoscopieCentrumColonCapaciteitVerdeling, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<ColoscopieCentrumColonCapaciteitVerdeling, String>(Model.of("Naam intakelocatie"), "coloscopieCentrum.naam"));
		columns.add(new PropertyColumn<ColoscopieCentrumColonCapaciteitVerdeling, String>(Model.of("Huidige adherentiepercentage"), "percentageCapaciteit")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public IModel<Object> getDataModel(IModel<ColoscopieCentrumColonCapaciteitVerdeling> rowModel)
			{
				return new Model(PercentageUtil.percentageToString(rowModel.getObject().getPercentageAdherentie()));
			}

		});
		columns.add(new AbstractColumn<ColoscopieCentrumColonCapaciteitVerdeling, String>(Model.of("Adherentiepercentage wijzigen"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<ColoscopieCentrumColonCapaciteitVerdeling>> cellItem, String componentId,
				IModel<ColoscopieCentrumColonCapaciteitVerdeling> rowModel)
			{
				cellItem.add(new AdherentieCellFragment(componentId, rowModel));
			}

		});
		columns.add(new PropertyColumn<ColoscopieCentrumColonCapaciteitVerdeling, String>(Model.of("Huidige capaciteitspercentage"), "percentageCapaciteit")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public IModel<Object> getDataModel(IModel<ColoscopieCentrumColonCapaciteitVerdeling> rowModel)
			{
				return new Model(PercentageUtil.percentageToString(rowModel.getObject().getPercentageCapaciteit()));
			}

		});
		columns.add(new AbstractColumn<ColoscopieCentrumColonCapaciteitVerdeling, String>(Model.of("Verwijderen"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<ColoscopieCentrumColonCapaciteitVerdeling>> cellItem, String componentId,
				final IModel<ColoscopieCentrumColonCapaciteitVerdeling> rowModel)
			{
				cellItem.add(new AjaxImageCellPanel<ColoscopieCentrumColonCapaciteitVerdeling>(componentId, rowModel, "icon-trash")
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected void onClick(AjaxRequestTarget target)
					{
						ColoscopieCentrumColonCapaciteitVerdeling verdeling = rowModel.getObject();
						ColoscopieCentrum intakelocatie = verdeling.getColoscopieCentrum();
						if (verdeling.getId() != null)
						{
							verwijderdeItemModels.put(intakelocatie.getId(), ModelUtil.sModel(verdeling));
						}
						else
						{
							UitnodigingsGebied uitnodigingsgebied = verdeling.getUitnodigingsGebied();
							uitnodigingsgebied.getVerdeling().remove(verdeling);
							intakelocatie.getCapaciteitVerdeling().remove(verdeling);
						}
						List<ColoscopieCentrum> locaties = intakelocatiesModel.getObject();
						locaties.add(intakelocatie);
						intakelocatiesModel.setObject(new ArrayList<>(locaties));
						newAdherentiePercentages.remove(ColonRestrictions.getUniekIdOf(verdeling));
						target.add(adherentieForm, intakelocaties, adherentieTabel);

					}
				});
			}
		});

		adherentieTabel = new ScreenitDataTable<ColoscopieCentrumColonCapaciteitVerdeling, String>("adherentie", columns,
			new SortableDataProvider<ColoscopieCentrumColonCapaciteitVerdeling, String>()
			{

				private static final long serialVersionUID = 1L;

				@Override
				public Iterator<? extends ColoscopieCentrumColonCapaciteitVerdeling> iterator(long first, long count)
				{
					return getVerdeling().subList(Ints.checkedCast(first), Ints.checkedCast(first + count)).iterator();
				}

				private List<ColoscopieCentrumColonCapaciteitVerdeling> getVerdeling()
				{
					List<ColoscopieCentrumColonCapaciteitVerdeling> verdeling = new ArrayList<>(getPageModel().getObject().getVerdeling());
					for (ColoscopieCentrumColonCapaciteitVerdeling verwijderdeItem : getVerwijderdeItems())
					{
						verdeling.remove(verwijderdeItem);
					}
					return verdeling;
				}

				@Override
				public long size()
				{
					return getVerdeling().size();
				}

				@Override
				public IModel<ColoscopieCentrumColonCapaciteitVerdeling> model(ColoscopieCentrumColonCapaciteitVerdeling object)
				{
					return ModelUtil.cModel(object);
				}

			}, new Model<>("intakelocatie(s)"));
		adherentieTabel.setOutputMarkupId(true);
		adherentieForm.add(adherentieTabel);

		adherentieForm.add(new IndicatingAjaxButton("toevoegen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				UitnodigingsGebied gebied = getPageModel().getObject();
				if (ModelUtil.nullSafeGet(intakelocatieModel) != null)
				{
					ColoscopieCentrum intakelocatie = intakelocatieModel.getObject();
					List<ColoscopieCentrum> locaties = intakelocatiesModel.getObject();
					locaties.remove(intakelocatie);
					intakelocatiesModel.setObject(new ArrayList<>(locaties));
					IModel<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeItem = verwijderdeItemModels.get(intakelocatie.getId());
					if (verwijderdeItem != null)
					{
						ColoscopieCentrumColonCapaciteitVerdeling verdeling = ModelUtil.nullSafeGet(verwijderdeItem);
						newAdherentiePercentages.put(ColonRestrictions.getUniekIdOf(verdeling), verdeling.getPercentageAdherentie());
						verwijderdeItemModels.remove(intakelocatie.getId());
					}
					else
					{
						ColoscopieCentrumColonCapaciteitVerdeling nieuweVerdeling = new ColoscopieCentrumColonCapaciteitVerdeling();
						List<ColoscopieCentrumColonCapaciteitVerdeling> verdeling = gebied.getVerdeling();
						verdeling.add(nieuweVerdeling);
						nieuweVerdeling = verdeling.get(verdeling.size() - 1); 
						nieuweVerdeling.setUitnodigingsGebied(gebied);

						nieuweVerdeling.setPercentageAdherentie(0);
						nieuweVerdeling.setPercentageCapaciteit(0);
						nieuweVerdeling.setColoscopieCentrum(intakelocatie);
						intakelocatie = nieuweVerdeling.getColoscopieCentrum(); 
						intakelocatie.getCapaciteitVerdeling().add(nieuweVerdeling);
						newAdherentiePercentages.put(ColonRestrictions.getUniekIdOf(nieuweVerdeling), 0);
					}
					setIntakelocatie(null);
					target.add(adherentieTabel, intakelocaties);
				}
				else
				{
					error(getString("geen.intakelocatie.geselecteerd"));
				}
			}

		});

		controleResultaatPanel = new EmptyPanel("controleResultaat");
		controleResultaatPanel.setOutputMarkupId(true);
		adherentieForm.add(controleResultaatPanel);

		adherentieForm.add(new IndicatingAjaxButton("controleren", adherentieForm)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				UitnodigingsGebied uitnodigingsGebied = (UitnodigingsGebied) getForm().getDefaultModelObject();
				List<CapaciteitsPercWijziging> capaciteitsPercWijzigingen = new ArrayList<>();
				try
				{
					capaciteitsPercWijzigingen = uitnodigingsGebiedService.bepaalCapaciteitsWijzigingen(uitnodigingsGebied, newAdherentiePercentages, getVerwijderdeItems());
				}
				catch (IllegalStateException e)
				{
					error(getString(e.getMessage(), null, e.getMessage()));
				}

				if (!hasErrorMessage())
				{
					ControleResultaatFragment newControleResultaatPanel = new ControleResultaatFragment(controleResultaatPanel.getId(), capaciteitsPercWijzigingen);
					controleResultaatPanel.replaceWith(newControleResultaatPanel);
					controleResultaatPanel = newControleResultaatPanel;
					target.add(controleResultaatPanel);
					target.appendJavaScript("$('.nieuweAdherentie').prop('disabled', true);");
					target.appendJavaScript("initTooltip();");
				}
			}
		});

	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("menu.beheer.gemeente.zoeken", GemeenteZoeken.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.beheer.gemeentegegevens", GemeenteGegevens.class)
		{
			private static final long serialVersionUID = 1L;

			@Override
			public IndicatingAjaxLink<?> createWicketLink(String markupId)
			{
				return new IndicatingAjaxLink<UitnodigingsGebied>(markupId, getPageModel())
				{
					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(AjaxRequestTarget target)
					{
						Gemeente gemeente = getModelObject().getGemeente();
						gemeente = hibernateService.load(Gemeente.class, gemeente.getId());
						setResponsePage(new GemeenteGegevens(ModelUtil.cRModel(gemeente)));
					}
				};
			}
		});
		contextMenuItems.add(new GebruikerMenuItem("menu.beheer.gebiedgegevens", false, GebiedGegevens.class));
		return contextMenuItems;
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return Boolean.TRUE;
	}

	public ColoscopieCentrum getIntakelocatie()
	{
		return ModelUtil.nullSafeGet(intakelocatieModel);
	}

	public void setIntakelocatie(ColoscopieCentrum intakelocatieModel)
	{
		this.intakelocatieModel = ModelUtil.sModel(intakelocatieModel);
	}

	private IModel<UitnodigingsGebied> getPageModel()
	{
		return (IModel<UitnodigingsGebied>) GebiedGegevens.this.getDefaultModel();
	}

	private List<ColoscopieCentrumColonCapaciteitVerdeling> getVerwijderdeItems()
	{
		List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeItems = new ArrayList<>();
		for (IModel<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeItem : verwijderdeItemModels.values())
		{
			verwijderdeItems.add(ModelUtil.nullSafeGet(verwijderdeItem));
		}
		return verwijderdeItems;
	}

	private void initAdherentiePercentages()
	{
		newAdherentiePercentages = new HashMap<>();
		for (ColoscopieCentrumColonCapaciteitVerdeling verdeling : getPageModel().getObject().getVerdeling())
		{
			newAdherentiePercentages.put(ColonRestrictions.getUniekIdOf(verdeling), verdeling.getPercentageAdherentie());
		}
		verwijderdeItemModels.clear();
	}

	@Override
	public void detachModels()
	{
		super.detachModels();
		ModelUtil.nullSafeDetach(intakelocatieModel);
		ModelUtil.nullSafeDetach(intakelocatiesModel);
		for (IModel<ColoscopieCentrumColonCapaciteitVerdeling> verdeling : verwijderdeItemModels.values())
		{
			ModelUtil.nullSafeDetach(verdeling);
		}
	}

	private static Boolean getGesplitsOpPostcode(IModel<UitnodigingsGebied> model)
	{
		UitnodigingsGebied gebied = model.getObject();
		Boolean gesplitsOpPostcode = null;
		if (gebied.getPostcodeGebied() != null)
		{
			gesplitsOpPostcode = Boolean.TRUE;
		}
		else if (StringUtils.isNotBlank(gebied.getWoonplaats()))
		{
			gesplitsOpPostcode = Boolean.FALSE;
		}
		return gesplitsOpPostcode;
	}

	private class AdherentieCellFragment extends Fragment
	{

		private static final long serialVersionUID = 1L;

		public AdherentieCellFragment(String id, IModel<ColoscopieCentrumColonCapaciteitVerdeling> model)
		{
			super(id, "adherentieFragment", fragments, model);

			add(new PercentageIntegerField("nieuweAdherentie", new MapModel<>(newAdherentiePercentages, ColonRestrictions.getUniekIdOf(model.getObject()))));
		}
	}

	private class ControleResultaatFragment extends Fragment
	{

		private static final long serialVersionUID = 1L;

		public ControleResultaatFragment(String id, final List<CapaciteitsPercWijziging> capaciteitsPercWijzigingen)
		{
			super(id, "controleResultaatFragment", fragments);

			final WebMarkupContainer tooltipContainter = new WebMarkupContainer("tooltipContainter");
			add(tooltipContainter);
			tooltipContainter.setOutputMarkupId(true);
			final RepeatingView tooltips = new RepeatingView("tooltip");
			tooltipContainter.add(tooltips);

			final Set<String> intakelocaties = new HashSet<>();
			Set<String> gebieden = new HashSet<>();
			final Map<String, BigDecimal> totaalIntakelocaties = new HashMap<>();
			final Map<String, BigDecimal> totaalGebieden = new HashMap<>();
			BigDecimal totaalNieuw = BigDecimal.ZERO;
			BigDecimal totaalOud = BigDecimal.ZERO;
			for (CapaciteitsPercWijziging wijziging : capaciteitsPercWijzigingen)
			{
				intakelocaties.add(wijziging.getIlId() + "|" + wijziging.getIntakelocatie());
				BigDecimal verschilOud = wijziging.getVerschilOud();
				if (verschilOud != null)
				{
					BigDecimal totaalIntakelocatie = totaalIntakelocaties.get("O" + wijziging.getIlId());
					if (totaalIntakelocatie == null)
					{
						totaalIntakelocatie = BigDecimal.ZERO;
					}

					totaalIntakelocaties.put("O" + wijziging.getIlId(), totaalIntakelocatie.add(verschilOud));
					BigDecimal totaalGebied = totaalGebieden.get("O" + wijziging.getUgId());
					if (totaalGebied == null)
					{
						totaalGebied = BigDecimal.ZERO;
					}

					totaalGebieden.put("O" + wijziging.getUgId(), totaalGebied.add(verschilOud));
					totaalOud = totaalOud.add(verschilOud);
				}
				BigDecimal verschilNieuw = wijziging.getVerschilNieuw();
				if (verschilNieuw != null)
				{
					BigDecimal totaalIntakelocatie = totaalIntakelocaties.get("N" + wijziging.getIlId());
					if (totaalIntakelocatie == null)
					{
						totaalIntakelocatie = BigDecimal.ZERO;
					}

					totaalIntakelocaties.put("N" + wijziging.getIlId(), totaalIntakelocatie.add(verschilNieuw));
					BigDecimal totaalGebied = totaalGebieden.get("N" + wijziging.getUgId());
					if (totaalGebied == null)
					{
						totaalGebied = BigDecimal.ZERO;
					}

					totaalGebieden.put("N" + wijziging.getUgId(), totaalGebied.add(verschilNieuw));
					totaalNieuw = totaalNieuw.add(verschilNieuw);
				}
				gebieden.add(wijziging.getUgId() + "|" + wijziging.getUitnodigingsgebied());

			}

			add(new ListView<String>("intakelocaties", new ArrayList<>(intakelocaties))
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void populateItem(ListItem<String> item)
				{
					String intakelocatie = item.getModelObject();
					item.add(new Label("naam", intakelocatie.split("\\|")[1]));
				}

			});
			add(new ListView<String>("oudNieuw", new ArrayList<>(intakelocaties))
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void populateItem(ListItem<String> item)
				{

				}

			});

			add(new ListView<String>("totaalIntakelocaties", new ArrayList<>(intakelocaties))
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void populateItem(ListItem<String> item)
				{
					String intakelocatie = item.getModelObject();
					Long intakelocatieId = Long.valueOf(intakelocatie.split("\\|")[0]);
					item.add(new Label("totaalVerschilNieuw", BigDecimalUtil.roundCapaciteit(totaalIntakelocaties.get("N" + intakelocatieId))));
					item.add(new Label("totaalVerschilOud", BigDecimalUtil.roundCapaciteit(totaalIntakelocaties.get("O" + intakelocatieId))));
				}

			});

			add(new ListView<String>("gebieden", new ArrayList<>(gebieden))
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void populateItem(ListItem<String> item)
				{
					String gebieden = item.getModelObject();
					String[] splittedGebied = gebieden.split("\\|");
					item.add(new Label("naam", splittedGebied[1]));
					final Long gebiedId = Long.valueOf(splittedGebied[0]);
					item.add(new ListView<String>("intakelocaties", new ArrayList<>(intakelocaties))
					{

						private static final long serialVersionUID = 1L;

						@Override
						protected void populateItem(ListItem<String> item)
						{
							String intakelocatie = item.getModelObject();
							Long intakelocatieId = Long.valueOf(intakelocatie.split("\\|")[0]);
							CapaciteitsPercWijziging curWijziging = null;
							for (CapaciteitsPercWijziging wijziging : capaciteitsPercWijzigingen)
							{
								if (wijziging.getIlId().equals(intakelocatieId) && wijziging.getUgId().equals(gebiedId))
								{
									curWijziging = wijziging;
								}
							}

							String verschilNieuwTekst = "";
							String verschilOudTekst = "";
							String tooltipId = null;
							if (curWijziging != null)
							{
								BigDecimal verschilNieuw = curWijziging.getVerschilNieuw();
								if (verschilNieuw != null)
								{
									verschilNieuwTekst = BigDecimalUtil.roundCapaciteit(verschilNieuw).toString();
								}
								else
								{
									verschilNieuwTekst = "N/A";
								}
								BigDecimal verschilOud = curWijziging.getVerschilOud();
								if (verschilOud != null)
								{
									verschilOudTekst = BigDecimalUtil.roundCapaciteit(verschilOud).toString();
								}
								else
								{
									verschilOudTekst = "N/A";
								}
								tooltipId = "tooltip-" + curWijziging.getUgId() + "_" + curWijziging.getIlId();
								tooltips.add(new Tooltip(tooltips.newChildId(), curWijziging));
							}

							Label labelVerschilNieuw = new Label("verschilNieuw", verschilNieuwTekst);
							item.add(labelVerschilNieuw);
							Label labelVerschilOud = new Label("verschilOud", verschilOudTekst);
							item.add(labelVerschilOud);
							if (tooltipId != null)
							{
								labelVerschilNieuw.add(new AttributeAppender("data-tooltip", Model.of(tooltipId)));
								labelVerschilOud.add(new AttributeAppender("data-tooltip", Model.of(tooltipId)));
							}
						}

					});

					item.add(new Label("totaalVerschilOud", BigDecimalUtil.roundCapaciteit(totaalGebieden.get("O" + gebiedId))));
					item.add(new Label("totaalVerschilNieuw", BigDecimalUtil.roundCapaciteit(totaalGebieden.get("N" + gebiedId))));
				}

			});

			add(new Label("totaalOud", BigDecimalUtil.roundCapaciteit(totaalOud)));
			add(new Label("totaalNieuw", BigDecimalUtil.roundCapaciteit(totaalNieuw)));

			IndicatingAjaxButton doorvoeren = new IndicatingAjaxButton("doorvoeren")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{

					dialog.openWith(target, new ConfirmPanel(IDialog.CONTENT_ID, new SimpleStringResourceModel("wijzigingenDoorvoeren"), null, new DefaultConfirmCallback()
					{

						private static final long serialVersionUID = 1L;

						@Override
						public void onYesClick(AjaxRequestTarget target)
						{
							IModel<UitnodigingsGebied> model = getPageModel();
							UitnodigingsGebied uitnodiginsgebied = model.getObject();
							uitnodigingsGebiedService.wijzigingenDoorvoeren(uitnodiginsgebied, newAdherentiePercentages, getVerwijderdeItems(), capaciteitsPercWijzigingen,
								ScreenitSession.get().getLoggedInInstellingGebruiker());
							ControleResultaatFragment fragment = ControleResultaatFragment.this;
							fragment.setVisible(false);
							target.add(fragment, adherentieTabel);
							info(getString("wijzigingen.doorgevoerd"));
							IModel<UitnodigingsGebied> nieuwModel = ModelUtil.cModel(hibernateService.load(UitnodigingsGebied.class, uitnodiginsgebied.getId()));
							adherentieForm.setDefaultModel(nieuwModel);
							initAdherentiePercentages();
							markeerFormulierenOpgeslagen(target);
						}

						@Override
						public void onNoClick(AjaxRequestTarget target)
						{
							super.onNoClick(target);
							target.appendJavaScript("$('.nieuweAdherentie').prop('disabled', true);");
						}

						@Override
						public void onCloseClick(AjaxRequestTarget target)
						{
							super.onCloseClick(target);
							target.appendJavaScript("$('.nieuweAdherentie').prop('disabled', true);");
						}

					}, dialog));

				}

				@Override
				protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
				{
					attributes.getAjaxCallListeners().add(0, new AjaxCallListener().onBefore("$('.nieuweAdherentie').prop('disabled', false);"));
				}

			};
			add(doorvoeren);
			doorvoeren.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_GEBIEDEN_ADHERENTIE_AANPASSEN, Actie.AANPASSEN));

			add(new AjaxLink<Void>("annuleren")
			{

				private static final long serialVersionUID = 1L;

				@Override
				public void onClick(AjaxRequestTarget target)
				{
					ControleResultaatFragment fragment = ControleResultaatFragment.this;
					fragment.setVisible(false);
					target.add(fragment);
					target.appendJavaScript("$('.nieuweAdherentie').prop('disabled', false);");
				}

			});
		}

		private class Tooltip extends Fragment
		{

			private static final long serialVersionUID = 1L;

			public Tooltip(String id, CapaciteitsPercWijziging wijziging)
			{
				super(id, "tooltipFragment", fragments);
				add(new AttributeAppender("class", Model.of(" tooltip-" + wijziging.getUgId() + "_" + wijziging.getIlId())));
				add(new Label("oud", PercentageUtil.percentageToString(wijziging.getOudCapPer())));
				add(new Label("oudBenIntakes", BigDecimalUtil.roundCapaciteit(wijziging.getOudBerekendeIntakes())));
				add(new Label("oudProgn", BigDecimalUtil.roundCapaciteit(wijziging.getOudIntakesProg())));
				add(new Label("oudVerschil", BigDecimalUtil.roundCapaciteit(wijziging.getVerschilOud())));
				add(new Label("new", PercentageUtil.percentageToString(wijziging.getNieuwCapPer())));
				add(new Label("newBenIntakes", BigDecimalUtil.roundCapaciteit(wijziging.getNieuwBerekendeIntakes())));
				add(new Label("newProgn", BigDecimalUtil.roundCapaciteit(wijziging.getNieuwIntakesProg())));
				add(new Label("newVerschil", BigDecimalUtil.roundCapaciteit(wijziging.getVerschilNieuw())));
			}
		}

	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(new PriorityHeaderItem(CssHeaderItem.forUrl("assets/js/libs/qtip/jquery.qtip.min.css")));
		response.render(new PriorityHeaderItem(JavaScriptHeaderItem.forUrl("assets/js/libs/qtip/jquery.qtip.min.js")));
		response.render(new OnDomReadyHeaderItem("initTooltip()"));
	}
}
