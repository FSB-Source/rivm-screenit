package nl.rivm.screenit.main.service;

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

import java.io.File;
import java.io.IOException;
import java.util.List;

import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.formulieren.TypeFormulier;
import nl.topicuszorg.formulieren2.persistence.instantie.FormulierInstantieImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.VraagInstantieImpl;

import org.apache.poi.openxml4j.exceptions.InvalidFormatException;

public interface FormulierService
{

	List<ScreenitFormulierInstantie> importFormulier(TypeFormulier typeFormulier, File file, String domein, boolean replace)
		throws InvalidFormatException, IOException, ClassNotFoundException;

	List<ScreenitFormulierInstantie> importFormulier(TypeFormulier typeFormulier, File file, String domein) throws InvalidFormatException, IOException, ClassNotFoundException;

	<T> VraagInstantieImpl<T> findVraagInstantieByIdentifier(FormulierInstantieImpl formulierInstantie, String identifier);

	ScreenitFormulierInstantie getFormulierInstatie(TypeFormulier typeFormulier);

}
