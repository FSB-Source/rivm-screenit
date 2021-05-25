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
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.PercentageIntegerField;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatiePaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
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

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxCallListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
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
public class AdherentieIntakelocatie extends GebiedenBeheerPage
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

	private IModel<UitnodigingsGebied> gebiedModel = new SimpleHibernateModel<UitnodigingsGebied>();

	private IModel<List<UitnodigingsGebied>> gebiedenModel;

	private BootstrapDialog dialog;

	private Map<Long, IModel<ColoscopieCentrumColonCapaciteitVerdeling>> verwijderdeItemModels = new HashMap<>();

	private ScreenitDataTable<ColoscopieCentrumColonCapaciteitVerdeling, String> adherentieTabel;

	private Form<ColoscopieCentrum> adherentieForm;

	public AdherentieIntakelocatie(IModel<ColoscopieCentrum> model)
	{
		setDefaultModel(model);

		add(new OrganisatiePaspoortPanel("paspoort", ModelUtil.sModel(model.getObject())));

		adherentieBeheer(model);

		fragments = new TransparentWebMarkupContainer("fragments");
		add(fragments);

		dialog = new BootstrapDialog("dialog");
		add(dialog);

	}

	private void adherentieBeheer(IModel<ColoscopieCentrum> model)
	{
		adherentieForm = new ScreenitForm<>("adherentieForm", model);
		adherentieForm.setOutputMarkupId(true);
		add(adherentieForm);

		List<UitnodigingsGebied> actieveGebieden = uitnodigingsGebiedService.getAllUitnodigingsgebieden();
		for (ColoscopieCentrumColonCapaciteitVerdeling verdeling : getPageModelObject().getCapaciteitVerdeling())
		{
			actieveGebieden.remove(verdeling.getUitnodigingsGebied());
		}
		gebiedenModel = ModelUtil.listRModel(actieveGebieden);
		final ScreenitDropdown<UitnodigingsGebied> uitnodigingsGebieden = ComponentHelper.newDropDownChoice("gebieden", gebiedenModel,
			new ChoiceRenderer<UitnodigingsGebied>("naam"));
		uitnodigingsGebieden.setModel(new CompoundPropertyModel<>(new PropertyModel<UitnodigingsGebied>(AdherentieIntakelocatie.this, "gebied")));
		adherentieForm.add(uitnodigingsGebieden);

		initAdherentiePercentages();

		List<IColumn<ColoscopieCentrumColonCapaciteitVerdeling, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<ColoscopieCentrumColonCapaciteitVerdeling, String>(Model.of("Naam uitnodigingsgebied"), "uitnodigingsGebied.naam"));
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
						UitnodigingsGebied gebied = verdeling.getUitnodigingsGebied();
						if (verdeling.getId() != null)
						{
							verwijderdeItemModels.put(gebied.getId(), ModelUtil.sModel(verdeling));
						}
						else
						{
							UitnodigingsGebied uitnodigingsgebied = verdeling.getUitnodigingsGebied();
							uitnodigingsgebied.getVerdeling().remove(verdeling);
							gebied.getVerdeling().remove(verdeling);
						}
						List<UitnodigingsGebied> gebieden = gebiedenModel.getObject();
						gebieden.add(gebied);
						gebiedenModel.setObject(new ArrayList<>(gebieden));
						newAdherentiePercentages.remove(ColonRestrictions.getUniekIdOf(verdeling));
						target.add(adherentieForm, uitnodigingsGebieden, adherentieTabel);

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
					List<ColoscopieCentrumColonCapaciteitVerdeling> verdeling = new ArrayList<>(getPageModelObject().getCapaciteitVerdeling());
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

			}, new Model<>("gebied(en)"));
		adherentieTabel.setOutputMarkupId(true);
		adherentieForm.add(adherentieTabel);

		adherentieForm.add(new IndicatingAjaxButton("toevoegen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ColoscopieCentrum intakelocatie = getPageModelObject();
				if (ModelUtil.nullSafeGet(gebiedModel) != null)
				{
					UitnodigingsGebied uitnodgingsGebied = gebiedModel.getObject();
					List<UitnodigingsGebied> gebieden = gebiedenModel.getObject();
					gebieden.remove(uitnodgingsGebied);
					gebiedenModel.setObject(new ArrayList<>(gebieden));
					IModel<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeItem = verwijderdeItemModels.get(uitnodgingsGebied.getId());
					if (verwijderdeItem != null)
					{
						ColoscopieCentrumColonCapaciteitVerdeling verdeling = ModelUtil.nullSafeGet(verwijderdeItem);
						newAdherentiePercentages.put(ColonRestrictions.getUniekIdOf(verdeling), verdeling.getPercentageAdherentie());
						verwijderdeItemModels.remove(uitnodgingsGebied.getId());
					}
					else
					{
						ColoscopieCentrumColonCapaciteitVerdeling nieuweVerdeling = new ColoscopieCentrumColonCapaciteitVerdeling();
						List<ColoscopieCentrumColonCapaciteitVerdeling> verdeling = intakelocatie.getCapaciteitVerdeling();
						verdeling.add(nieuweVerdeling);
						nieuweVerdeling = verdeling.get(verdeling.size() - 1); 
						nieuweVerdeling.setUitnodigingsGebied(uitnodgingsGebied);

						nieuweVerdeling.setPercentageAdherentie(0);
						nieuweVerdeling.setPercentageCapaciteit(0);
						nieuweVerdeling.setColoscopieCentrum(intakelocatie);
						uitnodgingsGebied = nieuweVerdeling.getUitnodigingsGebied(); 
						uitnodgingsGebied.getVerdeling().add(nieuweVerdeling);
						newAdherentiePercentages.put(ColonRestrictions.getUniekIdOf(nieuweVerdeling), 0);
					}
					setGebied(null);
					target.add(adherentieTabel, uitnodigingsGebieden);
				}
				else
				{
					error(getString("geen.gebied.geselecteerd"));
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
				ColoscopieCentrum intakelocatie = (ColoscopieCentrum) getForm().getDefaultModelObject();
				List<CapaciteitsPercWijziging> capaciteitsPercWijzigingen = new ArrayList<>();
				try
				{
					capaciteitsPercWijzigingen = uitnodigingsGebiedService.bepaalCapaciteitsWijzigingen(intakelocatie, newAdherentiePercentages, getVerwijderdeItems());
				}
				catch (IllegalStateException e)
				{
					String messageKey = e.getMessage();
					Object[] messageValues = {};
					if (messageKey.contains(","))
					{
						messageValues = messageKey.split(",");
						messageKey = (String) messageValues[0];
					}
					String message = getString(messageKey, null, messageKey);
					if (messageValues.length > 1)
					{
						message = String.format(message, messageValues);
					}
					error(message);
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
		contextMenuItems.add(new GebruikerMenuItem("menu.beheer.adherentieintakelocatie", false, AdherentieIntakelocatie.class));
		return contextMenuItems;
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return Boolean.TRUE;
	}

	public UitnodigingsGebied getGebied()
	{
		return ModelUtil.nullSafeGet(gebiedModel);
	}

	public void setGebied(UitnodigingsGebied gebied)
	{
		this.gebiedModel = ModelUtil.sModel(gebied);
	}

	private ColoscopieCentrum getPageModelObject()
	{
		return (ColoscopieCentrum) AdherentieIntakelocatie.this.getDefaultModelObject();
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
		for (ColoscopieCentrumColonCapaciteitVerdeling verdeling : getPageModelObject().getCapaciteitVerdeling())
		{
			newAdherentiePercentages.put(ColonRestrictions.getUniekIdOf(verdeling), verdeling.getPercentageAdherentie());
		}
		verwijderdeItemModels.clear();
	}

	@Override
	public void detachModels()
	{
		super.detachModels();
		ModelUtil.nullSafeDetach(gebiedModel);
		ModelUtil.nullSafeDetach(gebiedenModel);
		for (IModel<ColoscopieCentrumColonCapaciteitVerdeling> verdeling : verwijderdeItemModels.values())
		{
			ModelUtil.nullSafeDetach(verdeling);
		}
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
							ColoscopieCentrum intakelocatie = getPageModelObject();
							uitnodigingsGebiedService.wijzigingenDoorvoeren(intakelocatie, getVerwijderdeItems(), capaciteitsPercWijzigingen,
								ScreenitSession.get().getLoggedInInstellingGebruiker());
							ControleResultaatFragment fragment = ControleResultaatFragment.this;
							fragment.setVisible(false);
							target.add(fragment, adherentieTabel);
							info(getString("wijzigingen.doorgevoerd"));
							IModel<ColoscopieCentrum> nieuwModel = ModelUtil.cModel(hibernateService.load(ColoscopieCentrum.class, intakelocatie.getId()));
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
				add(new Label("oudAdh", PercentageUtil.percentageToString(wijziging.getOudAdhPer())));
				add(new Label("newAdh", PercentageUtil.percentageToString(wijziging.getNieuwAdhPer())));
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
