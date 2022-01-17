
package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

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

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;
import nl.rivm.screenit.main.service.mamma.MammaAfspraakService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaAfspraakZoekenPanel extends GenericPanel<Client>
{
	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private MammaBaseAfspraakService baseAfspraakService;

	@SpringBean
	private MammaAfspraakService afspraakService;

	@SpringBean
	private MammaBaseStandplaatsService baseStandplaatsService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	private IModel<MammaAfspraakWijzigenFilter> filterModel;

	private static final int OFFSET_TOTENMET_DATUM = 1;

	public MammaAfspraakZoekenPanel(String id, IModel<Client> clientModel)
	{
		super(id, clientModel);

		Client client = clientModel.getObject();

		LocalDate minimaleVanaf = dateSupplier.getLocalDate();
		Integer minimaleIntervalMammografieOnderzoeken = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_MAMMOGRAFIE_ONDERZOEKEN.name());

		boolean isClientportaal = ScreenitSession.get().checkPermission(Recht.CLIENT_DASHBOARD, Actie.INZIEN);

		if (isClientportaal)
		{
			Integer afsprakenVanafWerkdagen = preferenceService.getInteger(PreferenceKey.MAMMA_ONDERZOEKSCAPACITEIT_NIET_BESCHIKBAAR_BINNEN_WERKDAGEN.name(), 0);
			minimaleVanaf = DateUtil.plusWerkdagen(minimaleVanaf, afsprakenVanafWerkdagen);
		}

		LocalDate vanaf = baseAfspraakService.vroegstMogelijkeUitnodigingsDatum(client.getMammaDossier(), minimaleVanaf, minimaleIntervalMammografieOnderzoeken);
		LocalDate totEnmet = vanaf;

		if (isClientportaal)
		{
			totEnmet = totEnmet.plusMonths(OFFSET_TOTENMET_DATUM);
		}

		totEnmet = totEnmet.with(DayOfWeek.SUNDAY);

		MammaStandplaats standplaats;
		MammaScreeningsEenheid screeningsEenheid = null;

		MammaScreeningRonde screeningRonde = clientModel.getObject().getMammaDossier().getLaatsteScreeningRonde();
		MammaUitnodiging laatsteUitnodiging = screeningRonde != null ? screeningRonde.getLaatsteUitnodiging() : null;
		if (laatsteUitnodiging != null && laatsteUitnodiging.getLaatsteAfspraak() == null)
		{

			standplaats = laatsteUitnodiging.getStandplaatsRonde().getStandplaats();
			screeningsEenheid = laatsteUitnodiging.getStandplaatsRonde().getStandplaatsPerioden().stream()
				.min(Comparator.comparing(MammaStandplaatsPeriode::getScreeningsEenheidVolgNr))
				.map(MammaStandplaatsPeriode::getScreeningsEenheid)
				.orElse(null);
		}
		else
		{
			standplaats = baseStandplaatsService.getStandplaatsMetPostcode(client);
		}
		filterModel = new CompoundPropertyModel<>(new MammaAfspraakWijzigenFilter(vanaf, totEnmet, standplaats, screeningsEenheid));
		filterModel.getObject().setClient(client);

		List<IColumn<MammaKandidaatAfspraakDto, String>> columns = new ArrayList<>();
		if (!isClientportaal)
		{
			columns.add(new AbstractColumn<MammaKandidaatAfspraakDto, String>(Model.of("SE"))
			{
				private static final long serialVersionUID = 1L;

				@Override
				public void populateItem(Item<ICellPopulator<MammaKandidaatAfspraakDto>> cell, String id, IModel<MammaKandidaatAfspraakDto> kandidaatAfspraakDtoModel)
				{
					cell.add(
						new Label(id,
							hibernateService.load(MammaCapaciteitBlok.class, kandidaatAfspraakDtoModel.getObject().getCapaciteitBlokId()).getScreeningsEenheid().getNaam()));
				}
			});

			columns.add(new AbstractColumn<MammaKandidaatAfspraakDto, String>(Model.of("Standplaats"))
			{
				private static final long serialVersionUID = 1L;

				@Override
				public void populateItem(Item<ICellPopulator<MammaKandidaatAfspraakDto>> cell, String id, IModel<MammaKandidaatAfspraakDto> kandidaatAfspraakDtoModel)
				{
					cell.add(
						new Label(id, hibernateService.load(MammaStandplaatsPeriode.class, kandidaatAfspraakDtoModel.getObject().getStandplaatsPeriodeId()).getStandplaatsRonde()
							.getStandplaats().getNaam()));
				}
			});
		}
		else
		{
			columns.add(new AbstractColumn<MammaKandidaatAfspraakDto, String>(Model.of("Plaats"))
			{
				private static final long serialVersionUID = 1L;

				@Override
				public void populateItem(Item<ICellPopulator<MammaKandidaatAfspraakDto>> cell, String id, IModel<MammaKandidaatAfspraakDto> kandidaatAfspraakDtoModel)
				{
					MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class,
						kandidaatAfspraakDtoModel.getObject().getStandplaatsPeriodeId());
					MammaStandplaatsLocatie locatie = baseStandplaatsService.getStandplaatsLocatie(standplaatsPeriode.getStandplaatsRonde().getStandplaats(),
						DateUtil.toUtilDate(kandidaatAfspraakDtoModel.getObject().getDatum()));
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
		columns.add(new AbstractColumn<MammaKandidaatAfspraakDto, String>(new SimpleStringResourceModel("label.afstand"))
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<MammaKandidaatAfspraakDto>> cell, String id, IModel<MammaKandidaatAfspraakDto> kandidaatAfspraakDtoModel)
			{
				MammaKandidaatAfspraakDto afspraakDto = kandidaatAfspraakDtoModel.getObject();
				Double afstand = afspraakDto.getAfstand();
				if (afspraakDto.isAfstandOnbekend())
				{
					cell.add(new Label(id, "onbekend"));
				}
				else
				{
					cell.add(new Label(id, (int) Math.round(afstand)));
				}
			}
		});
		columns.add(new AbstractColumn<MammaKandidaatAfspraakDto, String>(new SimpleStringResourceModel("label.datum"))
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<MammaKandidaatAfspraakDto>> cell, String id, IModel<MammaKandidaatAfspraakDto> kandidaatAfspraakDtoModel)
			{
				cell.add(DateLabel.forDatePattern(id, Model.of(DateUtil.toUtilDate(kandidaatAfspraakDtoModel.getObject().getDatum())), "EEEE dd-MM-yyyy"));
			}
		});
		columns.add(new AbstractColumn<MammaKandidaatAfspraakDto, String>(new SimpleStringResourceModel("label.tijd"))
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<MammaKandidaatAfspraakDto>> cell, String id, IModel<MammaKandidaatAfspraakDto> kandidaatAfspraakDtoModel)
			{
				MammaKandidaatAfspraakDto kandidaatAfspraakDto = kandidaatAfspraakDtoModel.getObject();
				cell.add(DateLabel.forDatePattern(id, Model.of(DateUtil.toUtilDate(kandidaatAfspraakDto.getTijd(), kandidaatAfspraakDto.getDatum())), "HH:mm"));
			}
		});

		MammaKandidaatAfsprakenProvider kandidaatAfspraakProvider = new MammaKandidaatAfsprakenProvider(clientModel, filterModel);

		ScreenitDataTable<MammaKandidaatAfspraakDto, String> kandidaatAfspraken = new ScreenitDataTable<MammaKandidaatAfspraakDto, String>("kandidaatAfspraken", columns,
			kandidaatAfspraakProvider, 10, Model.of("opties"))
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaKandidaatAfspraakDto> kandidaatAfspraakDtoModel)
			{
				nieuweAfspraak(target, kandidaatAfspraakDtoModel.getObject(), filterModel.getObject().getVerzettenReden());

				String waarschuwing = afspraakService.controleerAfspraakInAndereLocatie(kandidaatAfspraakDtoModel.getObject(), clientModel.getObject().getMammaDossier());

				if (StringUtils.isNotBlank(waarschuwing))
				{
					warn(getString(waarschuwing));
				}
				this.setVisible(false);
				target.add(this);
			}
		};
		kandidaatAfspraken.setVisible(false);
		kandidaatAfspraken.setOutputMarkupPlaceholderTag(true);
		add(kandidaatAfspraken);

		add(new MammaAfspraakWijzigenFilterPanel("filter", filterModel, true, ModelUtil.sModel(standplaats))
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void zoeken(AjaxRequestTarget target)
			{
				kandidaatAfspraken.setVisible(true);
				target.add(kandidaatAfspraken);
			}
		});
	}

	protected abstract void nieuweAfspraak(AjaxRequestTarget target, MammaKandidaatAfspraakDto kandidaatAfspraakDto, MammaVerzettenReden mammaVerzettenReden);

	protected IModel<MammaAfspraakWijzigenFilter> getFilterModel()
	{
		return filterModel;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(filterModel);
	}
}
