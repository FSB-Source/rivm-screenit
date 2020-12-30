package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dto.mamma.afspraken.MammaStandplaatsPeriodeMetAfstandDto;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public abstract class MammaUitstelZoekenPanel extends Panel
{
	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private MammaBaseStandplaatsService baseStandplaatsService;

	@SpringBean
	private HibernateService hibernateService;

	public MammaUitstelZoekenPanel(String id, IModel<Client> clientModel)
	{
		super(id);
		Client client = clientModel.getObject();
		MammaStandplaats standplaats = baseStandplaatsService.getStandplaatsMetPostcode(client);
		MammaScreeningsEenheid screeningsEenheid = null;
		MammaStandplaatsRonde standplaatsRonde = client.getMammaDossier().getLaatsteScreeningRonde().getStandplaatsRonde();
		if (standplaats != null && standplaats.equals(standplaatsRonde.getStandplaats()))
		{
			screeningsEenheid = standplaatsRonde.getStandplaatsPerioden().get(0).getScreeningsEenheid();
		}

		IModel<MammaAfspraakWijzigenFilter> filterModel = new CompoundPropertyModel<>(new MammaAfspraakWijzigenFilter(null, null, null, screeningsEenheid));

		filterModel.getObject().setClient(clientModel.getObject());
		boolean isClientportaal = ScreenitSession.get().checkPermission(Recht.CLIENT_DASHBOARD, Actie.INZIEN);
		List<IColumn<MammaStandplaatsPeriodeMetAfstandDto, String>> columns = new ArrayList<>();
		if (!isClientportaal)
		{
			columns.add(new AbstractColumn<MammaStandplaatsPeriodeMetAfstandDto, String>(Model.of("SE"))
			{
				private static final long serialVersionUID = 1L;

				@Override
				public void populateItem(Item<ICellPopulator<MammaStandplaatsPeriodeMetAfstandDto>> cell, String id,
					IModel<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtoModel)
				{
					cell.add(
						new Label(id, hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeMetAfstandDtoModel.getObject().getStandplaatsPeriodeId())
							.getScreeningsEenheid().getNaam()));
				}
			});
			columns.add(new AbstractColumn<MammaStandplaatsPeriodeMetAfstandDto, String>(Model.of("Standplaats"))
			{
				private static final long serialVersionUID = 1L;

				@Override
				public void populateItem(Item<ICellPopulator<MammaStandplaatsPeriodeMetAfstandDto>> cell, String id,
					IModel<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtoModel)
				{
					cell.add(new Label(id, hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeMetAfstandDtoModel.getObject().getStandplaatsPeriodeId())
						.getStandplaatsRonde().getStandplaats().getNaam()));
				}
			});
		}
		if (isClientportaal)
		{
			columns.add(new AbstractColumn<MammaStandplaatsPeriodeMetAfstandDto, String>(Model.of("Plaats"))
			{
				private static final long serialVersionUID = 1L;

				@Override
				public void populateItem(Item<ICellPopulator<MammaStandplaatsPeriodeMetAfstandDto>> cell, String id,
					IModel<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtoModel)
				{
					MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class,
						standplaatsPeriodeMetAfstandDtoModel.getObject().getStandplaatsPeriodeId());
					MammaStandplaatsLocatie locatie = baseStandplaatsService.getStandplaatsLocatie(standplaatsPeriode.getStandplaatsRonde().getStandplaats(),
						standplaatsPeriode.getVanaf());
					if (locatie != null)
					{
						cell.add(new Label(id, locatie.getPlaats()));
					}
					else
					{
						cell.add(new Label(id, ""));
					}
				}
			});
		}
		columns.add(new AbstractColumn<MammaStandplaatsPeriodeMetAfstandDto, String>(new SimpleStringResourceModel("label.afstand"))
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<MammaStandplaatsPeriodeMetAfstandDto>> cell, String id,
				IModel<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtoModel)
			{
				MammaStandplaatsPeriodeMetAfstandDto standplaatsPeriodeMetAfstandDto = standplaatsPeriodeMetAfstandDtoModel.getObject();
				Double afstand = standplaatsPeriodeMetAfstandDto.getAfstand();
				if (standplaatsPeriodeMetAfstandDto.isOnbekendeAfstand())
				{
					cell.add(new Label(id, "onbekend"));
				}
				else
				{
					cell.add(new Label(id, (int) Math.round(afstand)));
				}
			}
		});
		columns.add(new AbstractColumn<MammaStandplaatsPeriodeMetAfstandDto, String>(new SimpleStringResourceModel("label.vanaf"))
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<MammaStandplaatsPeriodeMetAfstandDto>> cell, String id,
				IModel<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtoModel)
			{
				MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class,
					standplaatsPeriodeMetAfstandDtoModel.getObject().getStandplaatsPeriodeId());
				cell.add(DateLabel.forDatePattern(id, Model.of(standplaatsPeriode.getVanaf()), "EEEE dd-MM-yyyy"));
			}
		});
		columns.add(new AbstractColumn<MammaStandplaatsPeriodeMetAfstandDto, String>(new SimpleStringResourceModel("label.tot.en.met"))
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<MammaStandplaatsPeriodeMetAfstandDto>> cell, String id,
				IModel<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtoModel)
			{
				MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class,
					standplaatsPeriodeMetAfstandDtoModel.getObject().getStandplaatsPeriodeId());
				cell.add(DateLabel.forDatePattern(id, Model.of(standplaatsPeriode.getTotEnMet()), "EEEE dd-MM-yyyy"));
			}
		});

		MammaStandplaatsPeriodeProvider standplaatsPeriodeProvider = new MammaStandplaatsPeriodeProvider(clientModel, filterModel);

		ScreenitDataTable<MammaStandplaatsPeriodeMetAfstandDto, String> standplaatsPeriodes = new ScreenitDataTable<MammaStandplaatsPeriodeMetAfstandDto, String>(
			"standplaatsPeriodes", columns, standplaatsPeriodeProvider, 10, Model.of("standplaats periodes"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaStandplaatsPeriodeMetAfstandDto> model)
			{
				super.onClick(target, model);
				nieuwUitstel(target, model, filterModel.getObject().getVanaf());
				this.setVisible(false);
				target.add(this);
			}
		};
		standplaatsPeriodes.setVisible(false);
		standplaatsPeriodes.setOutputMarkupPlaceholderTag(true);
		add(standplaatsPeriodes);

		add(new MammaAfspraakWijzigenFilterPanel("filter", filterModel, false, null)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void zoeken(AjaxRequestTarget target)
			{
				MammaAfspraakWijzigenFilter filter = filterModel.getObject();
				filter.setTotEnMet(filter.getVanaf());
				standplaatsPeriodes.setVisible(true);
				target.add(standplaatsPeriodes);
			}
		});
	}

	protected abstract void nieuwUitstel(AjaxRequestTarget target, IModel<MammaStandplaatsPeriodeMetAfstandDto> model, Date zoekDatum);
}
