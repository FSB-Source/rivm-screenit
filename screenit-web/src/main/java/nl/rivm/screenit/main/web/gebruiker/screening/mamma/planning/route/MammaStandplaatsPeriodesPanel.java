package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route;

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

import java.text.SimpleDateFormat;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import nl.rivm.screenit.dto.mamma.planning.PlanningMeldingenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.main.service.mamma.MammaAfspraakService;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsPeriodeService;
import nl.rivm.screenit.main.util.StandplaatsPeriodeUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import nl.topicuszorg.wicket.input.behavior.IndicatingAjaxEventBehavior;
import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.head.CssHeaderItem;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.head.PriorityHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.googlecode.wicket.jquery.core.Options;
import com.googlecode.wicket.jquery.ui.interaction.sortable.Sortable;

public class MammaStandplaatsPeriodesPanel extends Panel
{

	private static final String TOOLTIP_MELDING_PREFIX = "tooltip-m-";

	private static final String TOOLTIP_BLOKKADE_PREFIX = "tooltip-b-";

	private static final long serialVersionUID = 1L;

	private static final String HANDLE = Options.asString(".handle");

	@SpringBean
	private MammaStandplaatsPeriodeService standplaatsPeriodeService;

	@SpringBean
	private MammaAfspraakService afspraakService;

	@SpringBean
	private MammaBaseAfspraakService baseAfspraakService;

	@SpringBean
	private HibernateService hibernateService;

	private BootstrapDialog dialog;

	private IModel<MammaScreeningsEenheid> huidigeScreeningsEenheid;

	private IModel<List<PlanningStandplaatsPeriodeDto>> huidigeStandplaatsPeriodes;

	private IModel<List<PlanningStandplaatsPeriodeDto>> vergelijkendeStandplaatsPeriode;

	private MammaStandplaatsPeriodeSortable sortable1;

	private MammaStandplaatsPeriodeSortable sortable2;

	private final WebMarkupContainer standplaatsPerioden1Container = new WebMarkupContainer("standplaatsPerioden1Container");

	private final WebMarkupContainer standplaatsPerioden2Container = new WebMarkupContainer("standplaatsPerioden2Container");

	private boolean magAanpassen;

	private RepeatingView tooltips;

	private Map<String, Fragment> addedTooltips = new HashMap<>();

	public MammaStandplaatsPeriodesPanel(String id, IModel<MammaScreeningsEenheid> model, BootstrapDialog dialog)
	{
		super(id, model);
		huidigeScreeningsEenheid = model;
		this.dialog = dialog;

		magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.AANPASSEN) && ScreenitSession.get().getScreeningOrganisatie() != null;

		standplaatsPerioden1Container.setOutputMarkupId(true);
		add(standplaatsPerioden1Container);

		standplaatsPerioden2Container.setOutputMarkupId(true);
		standplaatsPerioden2Container.setOutputMarkupPlaceholderTag(true);
		standplaatsPerioden2Container.setVisible(false);
		add(standplaatsPerioden2Container);

		initFirstSortable(model);

		final WebMarkupContainer tooltipContainer = new WebMarkupContainer("tooltipContainer");
		add(tooltipContainer);
		tooltipContainer.setOutputMarkupId(true);
		tooltips = new RepeatingView("tooltip");
		tooltipContainer.add(tooltips);

	}

	public MammaStandplaatsPeriodesPanel(String id, IModel<MammaScreeningsEenheid> model, IModel<MammaScreeningsEenheid> model2, BootstrapDialog dialog)
	{
		this(id, model, dialog);

		standplaatsPerioden2Container.setVisible(true);

		standplaatsPerioden1Container.add(new AttributeAppender("class", " split-table floatleft"));
		standplaatsPerioden2Container.add(new AttributeAppender("class", " split-table floatright"));

		initSecondSortable(model2);
	}

	private void initFirstSortable(IModel<MammaScreeningsEenheid> model)
	{
		huidigeStandplaatsPeriodes = new ListModel<PlanningStandplaatsPeriodeDto>(standplaatsPeriodeService.getStandplaatsPeriodesSorted(model.getObject()));

		sortable1 = this.newSortable("standplaatsPerioden", model.getObject(), huidigeStandplaatsPeriodes);
		sortable1.setOutputMarkupId(true);
		standplaatsPerioden1Container.add(sortable1);

		WebMarkupContainer handleHeader1 = new WebMarkupContainer("handleHeader1");
		handleHeader1.setVisible(magAanpassen);
		sortable1.add(handleHeader1);
	}

	private void initSecondSortable(IModel<MammaScreeningsEenheid> model2)
	{

		if (model2 == null)
		{
			WebMarkupContainer sortable2Placeholder = new WebMarkupContainer("standplaatsPerioden2");
			sortable2Placeholder.setVisible(false);
			standplaatsPerioden2Container.add(sortable2Placeholder);
		}
		else
		{
			vergelijkendeStandplaatsPeriode = new ListModel<PlanningStandplaatsPeriodeDto>(standplaatsPeriodeService.getStandplaatsPeriodesSorted(model2.getObject()));

			sortable2 = this.newSortable("standplaatsPerioden2", model2.getObject(), vergelijkendeStandplaatsPeriode);
			sortable2.setOutputMarkupId(true);
			sortable2.setOutputMarkupPlaceholderTag(true);
			standplaatsPerioden2Container.add(sortable2);

			WebMarkupContainer handleHeader2 = new WebMarkupContainer("handleHeader2");
			handleHeader2.setVisible(magAanpassen);
			sortable2.add(handleHeader2);

			sortable1.connectWith(sortable2);
			sortable2.connectWith(sortable1);
		}
	}

	private MammaStandplaatsPeriodeSortable newSortable(final String id, MammaScreeningsEenheid screeningsEenheid,
		final IModel<List<PlanningStandplaatsPeriodeDto>> list)
	{
		return new MammaStandplaatsPeriodeSortable(id, screeningsEenheid, list, new Options("handle", HANDLE).set("items", "'tr:not(.headers)'"))
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected HashListView<PlanningStandplaatsPeriodeDto> newListView(IModel<List<PlanningStandplaatsPeriodeDto>> model)
			{
				return MammaStandplaatsPeriodesPanel.this.newListView("items", model);
			}

			@Override
			protected void updateTooltips(AjaxRequestTarget target)
			{
				target.add(tooltips.getParent());
				target.appendJavaScript("initTooltip();");
			}

		};

	}

	protected Sortable.HashListView<PlanningStandplaatsPeriodeDto> newListView(String id, IModel<List<PlanningStandplaatsPeriodeDto>> model)
	{
		return new Sortable.HashListView<PlanningStandplaatsPeriodeDto>(id, model)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<PlanningStandplaatsPeriodeDto> item)
			{
				PlanningStandplaatsPeriodeDto standplaatsPeriodeDto = item.getModelObject();
				MammaStandplaats standplaats = hibernateService.load(MammaStandplaats.class, standplaatsPeriodeDto.standplaatsId);
				String naam = StandplaatsPeriodeUtil.getStandplaatsPeriodeNaam(standplaatsPeriodeDto, standplaats);
				Label labelStandplaats = new Label("standplaats", naam);
				Label afspraakDrempel = null;
				if (standplaatsPeriodeDto.afspraakDrempel != null)
				{
					afspraakDrempel = new Label("afspraakDrempel", standplaatsPeriodeDto.afspraakDrempel + "%");
				}
				else
				{
					afspraakDrempel = new Label("afspraakDrempel", "");
				}
				Label labelVanaf = new Label("vanaf", standplaatsPeriodeDto.vanaf);
				Label labelTotEnMet = new Label("totEnMet", standplaatsPeriodeDto.totEnMet);
				boolean totEnMetDatumDefinitief = (standplaatsPeriodeDto.totEnMet != null && !Boolean.TRUE.equals(standplaatsPeriodeDto.prognose));
				if (totEnMetDatumDefinitief)
				{
					labelTotEnMet.add(new AttributeAppender("style", Model.of("font-weight: bold;")));
				}

				addClickBehavior(labelStandplaats, item.getModel());
				addClickBehavior(afspraakDrempel, item.getModel());
				addClickBehavior(labelVanaf, item.getModel());
				addClickBehavior(labelTotEnMet, item.getModel());

				item.add(AttributeModifier.append("class", "ui-state-default"));
				item.add(labelStandplaats);
				item.add(afspraakDrempel);
				item.add(labelVanaf);
				item.add(labelTotEnMet);

				item.add(new MammaStandplaatsPeriodeOpmerkingenPanel("opmerkingen", item.getModel())
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected void openOpmerkingen(AjaxRequestTarget target, IModel<PlanningStandplaatsPeriodeDto> standplaatsPeriodeModel)
					{
						MammaStandplaats standplaats2 = hibernateService.get(MammaStandplaats.class, standplaatsPeriodeModel.getObject().standplaatsId);
						IModel<MammaStandplaats> standplaatsModel = ModelUtil.sModel(standplaats2);
						dialog.openWith(target, new MammaRouteStandplaatsOpmerkingenPanel(IDialog.CONTENT_ID, standplaatsModel)
						{

							private static final long serialVersionUID = 1L;

							@Override
							protected void close(AjaxRequestTarget target)
							{
								dialog.close(target);
							}
						});
					}
				});

				item.add(new MammaStandplaatsPeriodeBlokkadesPanel("blokkades", item.getModel())
				{

					@Override
					protected void addBlokkadeTooltip(WebMarkupContainer blokkades, IModel<PlanningStandplaatsPeriodeDto> standplaatsPeriodeModel)
					{
						String tooltipId = TOOLTIP_BLOKKADE_PREFIX + standplaatsPeriodeModel.getObject().conceptId;
						blokkades.add(new AttributeAppender("data-tooltip", Model.of(tooltipId)));
						Fragment fragment = addedTooltips.get(tooltipId);
						if (fragment == null)
						{
							fragment = new BlokkadesTooltip(tooltips.newChildId(), standplaatsPeriodeModel);
							tooltips.add(fragment);
						}
						else
						{
							BlokkadesTooltip replacement = new BlokkadesTooltip(fragment.getId(), standplaatsPeriodeModel);
							fragment.replaceWith(replacement);
							fragment = replacement;
						}
						addedTooltips.put(tooltipId, fragment);
					}

				});

				item.add(new MammaStandplaatsPeriodeIndicatiePanel("indicatie", item.getModel())
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected void addMeldingTooltip(WebMarkupContainer meldingen, IModel<PlanningStandplaatsPeriodeDto> standplaatsPeriodeModel)
					{
						String tooltipId = TOOLTIP_MELDING_PREFIX + standplaatsPeriodeModel.getObject().conceptId;
						meldingen.add(new AttributeAppender("data-tooltip", Model.of(tooltipId)));
						Fragment fragment = addedTooltips.get(tooltipId);
						if (fragment == null)
						{
							fragment = new MeldingenTooltip(tooltips.newChildId(), standplaatsPeriodeModel);
							tooltips.add(fragment);
						}
						else
						{
							MeldingenTooltip replacement = new MeldingenTooltip(fragment.getId(), standplaatsPeriodeModel);
							fragment.replaceWith(replacement);
							fragment = replacement;
						}
						addedTooltips.put(tooltipId, fragment);
					}
				});

				item.add(new MammaStandplaatsPeriodeIntervalPanel("interval", item.getModel())
				{

					private static final long serialVersionUID = 1L;
				});

				boolean magVerplaatsen = magAanpassen && !totEnMetDatumDefinitief
					&& (standplaatsPeriodeDto.id == null
						|| baseAfspraakService.countAfspraken(standplaatsPeriodeDto.id, MammaAfspraakStatus.NIET_GEANNULEERD.toArray(new MammaAfspraakStatus[] {})) == 0);
				item.add(new WebMarkupContainer("handle").setVisible(magVerplaatsen));
				item.add(new EmptyPanel("emptyHandle").setVisible(!magVerplaatsen));
			}

			private void addClickBehavior(Label label, IModel<PlanningStandplaatsPeriodeDto> standplaatsPeriodeIModel)
			{
				label.add(new IndicatingAjaxEventBehavior("click", label)
				{

					@Override
					protected void onEvent(AjaxRequestTarget target)
					{
						boolean magBeginDatumWijzigen = false;
						boolean magEindDatumWijzigen = false;
						IModel<PlanningStandplaatsPeriodeDto> standplaatsPeriodeModel = new CompoundPropertyModel<>(standplaatsPeriodeIModel.getObject());
						PlanningStandplaatsPeriodeDto standplaatsPeriodeDto = standplaatsPeriodeIModel.getObject();
						List<PlanningStandplaatsPeriodeDto> perioden = model.getObject();
						int huidigeStandplaatsPeriodeIndex = perioden.indexOf(standplaatsPeriodeDto);
						PlanningStandplaatsPeriodeDto volgendeStandplaatsPeriode = null;
						if (huidigeStandplaatsPeriodeIndex > 0)
						{
							magEindDatumWijzigen = !perioden.get(huidigeStandplaatsPeriodeIndex - 1).prognose;
						}
						else
						{
							magBeginDatumWijzigen = standplaatsPeriodeDto.screeningsEenheidVolgNr == 0
								&& (standplaatsPeriodeDto.id == null || baseAfspraakService.countAfspraken(standplaatsPeriodeDto.id, MammaAfspraakStatus.NIET_GEANNULEERD.toArray(new MammaAfspraakStatus[] {})) == 0);
							magEindDatumWijzigen = true;
						}
						if (perioden.size() > huidigeStandplaatsPeriodeIndex + 1)
						{
							volgendeStandplaatsPeriode = perioden.get(huidigeStandplaatsPeriodeIndex + 1);
						}

						dialog.openWith(target, new MammaStandplaatsPeriodeEditPanel(IDialog.CONTENT_ID, standplaatsPeriodeModel, volgendeStandplaatsPeriode,
							huidigeScreeningsEenheid, magBeginDatumWijzigen, magEindDatumWijzigen)
						{
							@Override
							protected void close(AjaxRequestTarget target)
							{
								sortable1.resetModel();
								if (sortable2 != null)
								{
									sortable2.resetModel();
								}
								target.add(standplaatsPerioden1Container);
								target.add(standplaatsPerioden2Container);
								target.appendJavaScript("initTooltip()");
								dialog.close(target);
							}

							@Override
							protected void standplaatsPeriodeGewijzigd(AjaxRequestTarget target)
							{
								sortable1.resetModel();
								if (sortable2 != null)
								{
									sortable2.resetModel();
								}
								target.add(standplaatsPerioden1Container);
								target.add(standplaatsPerioden2Container);
								addedTooltips.clear();
								tooltips.removeAll();
								target.add(tooltips.getParent());
								target.appendJavaScript("initTooltip()");
								dialog.close(target);
							}

						});
					}
				});
			}
		};

	}

	private class MeldingenTooltip extends Fragment
	{

		private static final long serialVersionUID = 1L;

		public MeldingenTooltip(String id, IModel<PlanningStandplaatsPeriodeDto> model)
		{
			super(id, "meldingenFragment", MammaStandplaatsPeriodesPanel.this);
			PlanningStandplaatsPeriodeDto standplaatsPeriodeDto = model.getObject();
			add(new AttributeAppender("class", Model.of(" " + TOOLTIP_MELDING_PREFIX + standplaatsPeriodeDto.conceptId)));
			add(new ListView<PlanningMeldingenDto.PlanningMeldingDto>("meldingen", standplaatsPeriodeDto.meldingenDto.meldingen)
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void populateItem(ListItem<PlanningMeldingenDto.PlanningMeldingDto> item)
				{
					item.setDefaultModel(new CompoundPropertyModel<>(item.getModel()));
					Label niveau = new Label("niveau", "");
					niveau.add(new AttributeAppender("class", " " + item.getModelObject().niveau.getCssClass()));
					item.add(niveau);
					item.add(new Label("tekst"));
				}
			});
		}
	}

	private class BlokkadesTooltip extends Fragment
	{

		private static final long serialVersionUID = 1L;

		public BlokkadesTooltip(String id, IModel<PlanningStandplaatsPeriodeDto> model)
		{
			super(id, "blokkadesFragment", MammaStandplaatsPeriodesPanel.this);
			PlanningStandplaatsPeriodeDto standplaatsPeriodeDto = model.getObject();
			add(new AttributeAppender("class", Model.of(" " + TOOLTIP_BLOKKADE_PREFIX + standplaatsPeriodeDto.conceptId)));
			List<MammaBlokkade> collect = standplaatsPeriodeDto.blokkadeIds.stream()
				.map(blokkadeId -> hibernateService.get(MammaBlokkade.class, blokkadeId))
				.sorted(Comparator.comparing(MammaBlokkade::getVanaf).thenComparing(MammaBlokkade::getTotEnMet))
				.collect(Collectors.toList());
			add(new ListView<MammaBlokkade>("blokkades", ModelUtil.listModel(collect))
			{

				@Override
				protected void populateItem(ListItem<MammaBlokkade> item)
				{
					MammaBlokkade blokkade = item.getModelObject();
					String label = null;
					switch (blokkade.getType())
					{
					case SCREENINGS_ORGANISATIE:
						label = blokkade.getRegio().getNaam();
						break;
					case SCREENINGS_EENHEID:
						label = blokkade.getScreeningsEenheid().getNaam();
						break;
					case STANDPLAATS:
						label = blokkade.getStandplaats().getNaam();
						break;
					}
					item.add(new Label("naam", label));
					SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
					item.add(new Label("vanaf", format.format(blokkade.getVanaf())));
					item.add(new Label("totEnMet", format.format(blokkade.getTotEnMet())));
				}
			});
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

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(vergelijkendeStandplaatsPeriode);
		ModelUtil.nullSafeDetach(huidigeStandplaatsPeriodes);
		ModelUtil.nullSafeDetach(huidigeScreeningsEenheid);
	}
}
