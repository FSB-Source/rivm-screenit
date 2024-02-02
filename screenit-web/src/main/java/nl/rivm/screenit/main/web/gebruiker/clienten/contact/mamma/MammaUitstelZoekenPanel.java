package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.DateUtil;
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
	private MammaBaseStandplaatsService baseStandplaatsService;

	@SpringBean
	private HibernateService hibernateService;

	protected MammaUitstelZoekenPanel(String id, IModel<Client> clientModel)
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
		List<IColumn<MammaStandplaatsPeriodeMetAfstandDto, String>> columns = new ArrayList<>();
		columns.add(new AbstractColumn<>(Model.of("SE"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaStandplaatsPeriodeMetAfstandDto>> cell, String id,
				IModel<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtoModel)
			{
				cell.add(
					new Label(id, hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeMetAfstandDtoModel.getObject().getStandplaatsPeriodeId())
						.getScreeningsEenheid().getNaam()));
			}
		});
		columns.add(new AbstractColumn<>(Model.of("Standplaats"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaStandplaatsPeriodeMetAfstandDto>> cell, String id,
				IModel<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtoModel)
			{
				cell.add(new Label(id, hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeMetAfstandDtoModel.getObject().getStandplaatsPeriodeId())
					.getStandplaatsRonde().getStandplaats().getNaam()));
			}
		});
		columns.add(new AbstractColumn<>(new SimpleStringResourceModel("label.afstand"))
		{
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
		columns.add(new AbstractColumn<>(new SimpleStringResourceModel("label.vanaf"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaStandplaatsPeriodeMetAfstandDto>> cell, String id,
				IModel<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtoModel)
			{
				MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class,
					standplaatsPeriodeMetAfstandDtoModel.getObject().getStandplaatsPeriodeId());
				cell.add(DateLabel.forDatePattern(id, Model.of(standplaatsPeriode.getVanaf()), "EEEE dd-MM-yyyy"));
			}
		});
		columns.add(new AbstractColumn<>(new SimpleStringResourceModel("label.tot.en.met"))
		{
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

		ScreenitDataTable<MammaStandplaatsPeriodeMetAfstandDto, String> standplaatsPeriodes = new ScreenitDataTable<>(
			"standplaatsPeriodes", columns, standplaatsPeriodeProvider, 10, Model.of("standplaats periodes"))
		{

			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaStandplaatsPeriodeMetAfstandDto> model)
			{
				super.onClick(target, model);
				nieuwUitstel(target, model, DateUtil.toUtilDate(filterModel.getObject().getVanaf()));
				this.setVisible(false);
				target.add(this);
			}
		};
		standplaatsPeriodes.setVisible(false);
		standplaatsPeriodes.setOutputMarkupPlaceholderTag(true);
		add(standplaatsPeriodes);

		add(new MammaAfspraakWijzigenFilterPanel("filter", filterModel, true, null)
		{
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
