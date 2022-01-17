
package nl.rivm.screenit.main.web.gebruiker.screening.colon.intake;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ColoscopieCentrumWrapper;
import nl.rivm.screenit.model.colon.ColoscopieCentrumZoekCriteria;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.planning.web.component.DateTimeField;
import nl.topicuszorg.wicket.planning.web.component.TimeField;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MaakAfspraakZonderCapaciteitPanel extends GenericPanel<ColonIntakeAfspraak>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private OrganisatieZoekService organisatieZoekService;

	@SpringBean
	private HibernateService hibernateService;

	private Component container;

	private Component nieuweAfspraakContainer;

	private int niveau = 0;

	private final boolean readOnly;

	public MaakAfspraakZonderCapaciteitPanel(String id, boolean readOnly, IModel<ColonIntakeAfspraak> model)
	{
		super(id, model);
		this.readOnly = readOnly;

		container = new WijzigColoscopiecentrum("container");
		container.setVisible(!readOnly);
		container.setOutputMarkupId(true);
		add(container);

		addOrReplaceNieuweAfspraak(null);

	}

	private void close(AjaxRequestTarget target)
	{
		replaceContainerWith(target, new EmptyPanel(container.getId()));
	}

	private void replaceContainerWith(AjaxRequestTarget target, final WebMarkupContainer nieuwContainer)
	{
		nieuwContainer.setOutputMarkupId(true);
		container.replaceWith(nieuwContainer);
		container = nieuwContainer;
		target.add(nieuwContainer);
	}

	private void addOrReplaceNieuweAfspraak(AjaxRequestTarget target)
	{
		WebMarkupContainer newNieuweAfspraakContainer = new WebMarkupContainer("nieuweAfspraak");

		newNieuweAfspraakContainer.add(new Label("conclusie.locatieNieuweAfspraak.coloscopieCentrum.naam"));
		newNieuweAfspraakContainer.add(new Label("conclusie.locatieNieuweAfspraak.name"));
		newNieuweAfspraakContainer.add(new Label("conclusie.locatieNieuweAfspraak.coloscopieCentrum.locatieBeschrijving"));
		newNieuweAfspraakContainer.add(new IndicatingAjaxLink<ColonIntakeAfspraak>("coloscopieCentrumWijzigen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				replaceContainerWith(target, new WijzigColoscopiecentrum(container.getId()));
			}

		});

		DateTimeField datumTijd = new DateTimeField("conclusie.datumTijdNieuweAfspraak")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected TimeField newTimeField(String wicketId, IModel<Date> model)
			{
				TimeField timeField = new TimeField(wicketId, model)
				{
					private static final long serialVersionUID = 1L;

					@Override
					public void convertInput()
					{
						Integer m = getMinutesField().getConvertedInput();
						Integer h = getHoursField().getConvertedInput();

						super.convertInput();
						if (h == null || m == null)
						{
							setConvertedInput(null);
						}
					}
				};
				return timeField;
			}

		};

		newNieuweAfspraakContainer.add(datumTijd.setRequired(true));
		datumTijd.setVisible(niveau > 0);

		newNieuweAfspraakContainer.setOutputMarkupId(true);
		if (target != null)
		{
			nieuweAfspraakContainer.replaceWith(newNieuweAfspraakContainer);
			target.add(newNieuweAfspraakContainer);
		}
		else
		{
			add(newNieuweAfspraakContainer);
		}
		newNieuweAfspraakContainer.setVisible(!readOnly);
		nieuweAfspraakContainer = newNieuweAfspraakContainer;
	}

	private class WijzigColoscopiecentrum extends Fragment
	{

		private static final long serialVersionUID = 1L;

		private WebMarkupContainer resultatenContainer;

		public WijzigColoscopiecentrum(String id)
		{
			super(id, "centraZoekenContainer", MaakAfspraakZonderCapaciteitPanel.this);

			ColoscopieCentrumZoekCriteria zoekCriteria = new ColoscopieCentrumZoekCriteria();
			IModel<ColoscopieCentrumZoekCriteria> model = new CompoundPropertyModel<>(zoekCriteria);
			ScreenitForm<ColoscopieCentrumZoekCriteria> form = new ScreenitForm<>("innerForm", model);

			Integer[] afstanden = new Integer[] { 5, 10, 15, 20, 25, 30, 35, 40, 45 };
			form.add(new DropDownChoice<>("afstand", Arrays.asList(afstanden)).setNullValid(true));
			add(new IndicatingAjaxButton("centraZoekenAnnuleren", form)
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					close(target);
				}
			});
			IndicatingAjaxButton centraZoeken = new IndicatingAjaxButton("centraZoeken", form)
			{
				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					replaceWithList(target, (ColoscopieCentrumZoekCriteria) form.getModelObject());
				}
			};
			add(centraZoeken);

			TextField<Object> naam = new TextField<>("naam");
			naam.add(
				new AttributeModifier("onkeypress", Model.of("if (event.keyCode == 13) { document.getElementById('" + centraZoeken.getMarkupId() + "').click(); return false;}")));
			form.add(naam);
			TextField<Object> plaats = new TextField<>("plaats");
			plaats.add(
				new AttributeModifier("onkeypress", Model.of("if (event.keyCode == 13) { document.getElementById('" + centraZoeken.getMarkupId() + "').click(); return false;}")));
			form.add(plaats);
			replaceWithList(null, null);

			add(form);

		}

		private void replaceWithList(AjaxRequestTarget target, final ColoscopieCentrumZoekCriteria zoekObject)
		{

			WebMarkupContainer newResultatenContainer = new WebMarkupContainer("resultatenContainer");
			List<ColoscopieCentrumWrapper> wrappers = new ArrayList<>();
			List<ColoscopieCentrumWrapper> kamerWrappers = new ArrayList<>();
			if (target != null)
			{
				wrappers = organisatieZoekService.zoekIntakeLocaties(zoekObject, MaakAfspraakZonderCapaciteitPanel.this.getModelObject().getClient(), false);

				for (ColoscopieCentrumWrapper wrapper : wrappers)
				{
					ColoscopieCentrum intakeLocatie = hibernateService.load(ColoscopieCentrum.class, wrapper.getId());
					for (Kamer kamer : intakeLocatie.getKamers())
					{
						ColoscopieCentrumWrapper nieuweWrapper = new ColoscopieCentrumWrapper();
						nieuweWrapper.setId(kamer.getId());
						nieuweWrapper.setNaam(intakeLocatie.getNaam());
						nieuweWrapper.setKamer(kamer.getName());
						nieuweWrapper.setAfstand(wrapper.getAfstand());
						nieuweWrapper.setPlaats(wrapper.getPlaats());
						kamerWrappers.add(nieuweWrapper);
					}

				}
			}
			ListView<ColoscopieCentrumWrapper> centra = new ListView<ColoscopieCentrumWrapper>("centraResultaten", kamerWrappers)
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void populateItem(final ListItem<ColoscopieCentrumWrapper> item)
				{
					item.setModel(new CompoundPropertyModel<ColoscopieCentrumWrapper>(item.getModel()));

					item.add(new Label("naam"));
					item.add(new Label("kamer"));
					item.add(new Label("plaats"));
					item.add(new Label("afstand"));
					item.add(new AjaxEventBehavior("click")
					{
						private static final long serialVersionUID = 1L;

						@Override
						protected void onEvent(AjaxRequestTarget target)
						{
							ColoscopieCentrumWrapper kamerWrapper = item.getModelObject();
							Kamer kamer = hibernateService.load(Kamer.class, kamerWrapper.getId());
							MaakAfspraakZonderCapaciteitPanel.this.getModelObject().getConclusie().setLocatieNieuweAfspraak(kamer);
							niveau++;
							addOrReplaceNieuweAfspraak(target);
							close(target);
						}

					});
				}
			};
			newResultatenContainer.add(centra);
			newResultatenContainer.setOutputMarkupId(true);
			if (target != null)
			{
				resultatenContainer.replaceWith(newResultatenContainer);
				target.add(newResultatenContainer);
			}
			else
			{
				add(newResultatenContainer);
			}
			resultatenContainer = newResultatenContainer;
		}
	}

}
